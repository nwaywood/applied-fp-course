{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Level04.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Bifunctor                     (first, second)
import           Data.Time                          (UTCTime, getCurrentTime)

import           Database.SQLite.Simple             (Connection, Query (Query))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level04.Types                      (Comment(..), CommentText,
                                                     Error, Topic, getTopic, getCommentText, fromDBComment)
import Level04.Types.Error (Error (..))

import Level04.DB.Types (DBComment)

-- ------------------------------------------------------------------------------|
-- You'll need the documentation for sqlite-simple & sqlite-simple-errors handy! |
-- ------------------------------------------------------------------------------|

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
--
-- To help with that, we create a new data type that can hold our `Connection`
-- for us, and allows it to be expanded later if we need to
data FirstAppDB = FirstAppDB
  { dbConn :: Connection
  }

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB fappDB =
  Sql.close $ dbConn fappDB

-- Given a `FilePath` to our SQLite DB file, initialise the database and ensure
-- our Table is there by running a query to create it, if it doesn't exist
-- already.
initDB
  :: FilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp =
  Sql.runDBAction $
    Sql.open fp >>= (\conn -> const (pure $ FirstAppDB conn) (Sql.execute_ conn createTableQ))
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ = "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time TEXT)"

-- Note that we don't store the `Comment` in the DB, it is the type we build
-- to send to the outside world. We will be loading our `DBComment` type from
-- the FirstApp.DB.Types module before converting trying to convert it to a
-- `Comment`.
--
-- To go from a DBComment to a Comment, we need to use ``fromDBComment`` that is
-- defined in FirstApp.Types.
--
-- HINT: You can use '?' or named place-holders as query parameters. Have a look
-- at the section on parameter substitution in sqlite-simple's documentation.
-- There are several possible implementations of this function. Particularly
-- there may be a trade-off between deciding to throw an Error if a DBComment
-- cannot be converted to a Comment, or simply ignoring any DBComment that is
-- not valid.
getComments
  :: FirstAppDB
  -> Topic
  -> IO (Either Error [Comment])
getComments firstAppDB topic =
  let
    sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
    getDBComments :: IO [DBComment]
    getDBComments = Sql.query (dbConn firstAppDB) sql (Sql.Only $ getTopic topic)
    in
       (traverse fromDBComment =<<) <$> (first DBError <$> Sql.runDBAction getDBComments)

addCommentToTopic
  :: FirstAppDB
  -> Topic
  -> CommentText
  -> IO (Either Error ())
-- addCommentToTopic db topic comment =
--   let
--     sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
--     addComment :: IO ()
--     addComment = Sql.execute (dbConn db) sql (getTopic topic, getCommentText comment, 123 :: Int)
--   in
--     first DBError <$> Sql.runDBAction addComment
addCommentToTopic db topic comment =
  let
    sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
    addComment :: UTCTime -> IO ()
    addComment t = Sql.execute (dbConn db) sql (getTopic topic, getCommentText comment, t)
  in
    getCurrentTime >>= (\time ->
        first DBError <$> Sql.runDBAction (addComment time))

getTopics
  :: FirstAppDB
  -> IO (Either Error [Topic])
getTopics db =
  let
    sql = "SELECT DISTINCT topic FROM comments"
    getTopics_ :: IO [DBComment]
    getTopics_ = Sql.query_ (dbConn db) sql
  in
    second (commentTopic <$>) <$> ((traverse fromDBComment =<<) <$> (first DBError <$> Sql.runDBAction getTopics_))

deleteTopic
  :: FirstAppDB
  -> Topic
  -> IO (Either Error ())
deleteTopic db topic =
  let
    sql = "DELETE FROM comments WHERE topic = ?"
    deleteTopic_ :: IO ()
    deleteTopic_ = Sql.execute (dbConn db) sql (Sql.Only $ getTopic topic)
  in
    first DBError <$> Sql.runDBAction deleteTopic_
