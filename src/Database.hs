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

project :: Connection -> Int -> IO (Maybe Project)
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
                                  order by billable; |]

employeeHours :: Connection -> Int -> Month -> Year -> IO [EmployeeHours]
employeeHours conn pid month year =
  let fetchQuery = [sql| select e.first_name || ' ' || e.last_name, (
                          select array_agg(coalesce(t.sum,0))
                          from generate_series(date ?, date ?, '1 day'::interval) i
                          left join (select date, sum(minutes) as sum
                                      from time_entry
                                      where employee = t.employee
                                        and project = ?
                                        and date >= date ?
                                        and date <= date ?
                                      group by date ) t on t.date = i::date )
                         from time_entry t, employees e
                         where t.employee = e.id
                         group by t.employee, e.first_name, e.last_name; |]
  in query conn fetchQuery (startDate, endDate, pid, startDate, endDate)
  where startDate, endDate :: Text
        startDate = T.pack $ show year <> "-" <> show month <> "-1"
        endDate   = T.pack $ show year <> "-" <> show month <> "-" <> show (lastDay month year)

projectHours :: Connection -> Int -> Month -> Year -> IO ProjectHours
projectHours conn pid mon year = do
  (Just p) <- project conn pid
  hs <- employeeHours conn pid mon year
  return $ ProjectHours p hs

type Month = Int
type Year = Int
type Day = Int

-- returns the last day for a given month and year
lastDay :: Month -> Year -> Day
lastDay month year
  | month `elem` [1,3,5,7,8,10,12] = 31
  | month `elem` [4,6,9,11]        = 30
  | month == 2 = if year `mod` 4 == 0 && (year `mod` 100 /= 0 || year `mod` 400 == 0)
                 then 29
                 else 28
lastDay _ _ = error "dates are hard"
