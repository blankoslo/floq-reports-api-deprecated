{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language QuasiQuotes #-}

module Database
  (
    project
  , projects
  , employeeHours
  , projectHours
  , timeTrackingStatus
  ) where

import Types (Project, EmployeeHours, ProjectHours(ProjectHours), TimeTrackingStatus(TimeTrackingStatus))

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time (parseDate)
import Database.PostgreSQL.Simple.SqlQQ (sql)

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

employeeHours :: Connection -> Text -> Day -> Day -> IO [EmployeeHours]
employeeHours conn pid startDate endDate = do
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
                              and date <= date ?) > 0
                        order by e.first_name, e.last_name;
                   |]
  query conn fetchQuery (startDate, endDate, pid, startDate, endDate, pid, startDate, endDate)

projectHours :: Connection -> Text -> Day -> Day -> IO (Maybe ProjectHours)
projectHours conn pid startDate endDate =
  project conn pid >>= \case
    Just p -> do
      hs <- employeeHours conn pid startDate endDate
      return . Just $ ProjectHours p hs
    Nothing -> return Nothing

timeTrackingStatus :: Connection -> ByteString -> ByteString -> IO TimeTrackingStatus
timeTrackingStatus conn startDate endDate = do
  let Right start = parseDate startDate
  let Right end = parseDate endDate
  TimeTrackingStatus <$> query conn [sql| select * from time_tracking_status(?, ?) |] (start, end)
