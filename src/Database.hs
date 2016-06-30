{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language QuasiQuotes #-}

module Database
  (
    project
  , projects
  , employeeHours
  , projectHours
  ) where

import Types (Project, EmployeeHours, ProjectHours(ProjectHours))

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ (sql)

type Month = Int
type Year = Int
type Day = Int

project :: Connection -> Text -> IO (Maybe Project)
project conn pid = do
  let selectQuery = [sql| select p.id, p.name, c.name
                          from projects p, customers c
                          where p.id = ? and p.customer = c.id
                          order by billable; |]
  ps <- query conn selectQuery [pid]
  case ps of
    [p] -> return (Just p)
    _   -> return Nothing

projects :: Connection -> IO [Project]
projects conn = query_ conn [sql| select p.id, p.name, c.name
                                  from projects p, customers c
                                  where p.customer = c.id
                                  order by billable desc, id; |]

employeeHours :: Connection -> Text -> Month -> Year -> IO [EmployeeHours]
employeeHours conn pid month year = do
  let fetchQuery = [sql|select e.first_name || ' ' || e.last_name as name, (
                            select array_agg(coalesce(t.sum, 0)) :: float8[]
                            from generate_series(date ?, date ?, '1 day'::interval) i
                            left join ( -- select (date, hours) pair for employee
                                     select date, round(sum(minutes)/60.0, 1) as sum
                                     from time_entry
                                     where employee = e.id
                                       and project = ?
                                       and date >= date ?
                                       and date <= date ?
                                       group by date
                                 ) t on t.date = i::date
                            )
                        from employees e
                        where (
                            select sum(t.minutes)
                            from time_entry t
                            where e.id = t.employee
                              and project = ?
                              and date >= date ?
                              and date <= date ?) > 0;
                   |]
  query conn fetchQuery (startDate, endDate, pid, startDate, endDate, pid, startDate, endDate)
  where startDate, endDate :: Text
        startDate = T.pack $ show year <> "-" <> show month <> "-1"
        endDate   = T.pack $ show year <> "-" <> show month <> "-" <> show (lastDay month year)

projectHours :: Connection -> Text -> Month -> Year -> IO ProjectHours
projectHours conn pid mon year = do
  (Just p) <- project conn pid
  hs <- employeeHours conn pid mon year
  return $ ProjectHours p hs


-- returns the last day for a given month and year
lastDay :: Month -> Year -> Day
lastDay month year
  | month `elem` [1,3,5,7,8,10,12] = 31
  | month `elem` [4,6,9,11]        = 30
  | month == 2 = if year `mod` 4 == 0 && (year `mod` 100 /= 0 || year `mod` 400 == 0)
                 then 29
                 else 28
lastDay _ _ = error "dates are hard"
