-- -----------------------------------------------------
    
-- IMPORTS
    
-- -----------------------------------------------------

-- For working with the command line environment.
import System.Environment

-- For exiting programs cleanly.
import System.Exit

-- For working with the filesystem.
import System.Directory

-- For doing I/O with the system.
import System.IO

-- For working with lists.
import Data.List

-- For helpful control structures.
import Control.Monad

-- For reading byte streams strictly.
import qualified Data.ByteString as Strict

-- For reading byte streams lazily.
import qualified Data.ByteString.Lazy as Lazy

-- For using cryptographic hashes.
import Crypto.Hash.SHA1 as Hash

-- For using printf to format strings.
import Text.Printf (printf)

-- For using `Maybe`.
import Data.Maybe

-- For working with strings.
import Data.String.Utils


-- -----------------------------------------------------
    
-- MAIN
    
-- -----------------------------------------------------

-- The main entry point for the program.
main = do

    -- Get all arguments that were passed to this program.
    given_arguments <- getArgs
    
    -- Users can type something like this:
    -- 
    --   $ repo print hello
    -- 
    -- We want to treat `print` as a command, and `hello` 
    -- as an argument for `print`. So, let's treat the 
    -- first argument after `repo` as a command, and the
    -- rest as arguments for that command. 
    let given_command = first_of given_arguments
    let arguments_for_the_command = rest_of given_arguments

    -- Look up the action that corresponds to the given command.
    let command = lookup given_command list_of_commands 
        
    -- If there was no command, display a message.
    if unrecognized command
       then display "I do not recognize that command."
       else command `run_with` arguments_for_the_command

    -- Exit the program cleanly.
    exit



-- -----------------------------------------------------
    
-- COMMANDS LIST
    
-- -----------------------------------------------------

-- A list of commands, and the functions to execute for each one.
list_of_commands = [ ("print", print_message),
                     ("init", initialize_repo),
                     ("status", status),
                     ("stage", stage),
                     ("unstage", unstage),
                     ("commit", commit),
                     ("branches", branches),
                     ("branch", branch),
                     ("checkout", checkout),
                     ("merge", merge_in)
                   ]



-- -----------------------------------------------------
    
-- IMPLEMENTATION OF COMMANDS
    
-- -----------------------------------------------------

-- Print a message to the command line.
print_message [message] = print message

-- Initialize the repo.
initialize_repo [] = do

    -- Is there a .repo directory? 
    repo_already <- doesDirectoryExist ".repo"
    
    -- If so, display a message.
    -- Otherwise, create it.
    if repo_already
        then display "The repo already exists."
        else do
      
            -- We'll store everything inside a hidden folder called ".repo".
            createDirectory ".repo" 
            
            -- We'll store all versions of all files here.
            createDirectory ".repo/files" 
            
            -- We'll store snapshots of the filesystem at different 
            -- points in history here.
            createDirectory ".repo/snapshots" 
            
            -- We'll store commits here.
            createDirectory ".repo/commits"
            
            -- We'll store references to branches here. 
            createDirectory ".repo/branches"
            
            -- We'll start with a default "master" branch.
            writeFile ".repo/branches/master" ""
                        
            -- We'll keep a reference to the current point in history here.
            -- To start, we'll point it at the master branch.
            writeFile ".repo/current-point" "master"
            
            -- We'll build up the list of files we want to commit here.
            writeFile ".repo/stage" ""
            
            -- Finally, display a "success" message.
            display "Initialization finished. Empty repo created."

-- Add a file to the list of files to be committed.
stage [file] = do

    -- Is this a real file? Does it exist?
    file_exists <- doesFileExist file
    
    -- If not, display a message. Otherwise, proceed.
    if not file_exists
        then display "That file does not exist."
        else do
            
            -- Get a hash of the file.
            hash <- hash_of file
            
            -- Add the hash and filename to the stage.
            appendFile ".repo/stage" (hash ++ " " ++ file ++ "\n")
            
            -- Display a message about what we've added.
            display ("-- " ++ file ++ " added to the stage.")

    -- Finish the IO activity. 
    finish_IO

-- Remove a file from the list of files to be committed.
unstage [file] = do

    -- Is this a real file? Does it exist?
    file_exists <- doesFileExist file
    
    -- If not, display a message. Otherwise, proceed.
    if not file_exists
        then display "That file does not exist."
        else do

            -- Get a hash of the file.
            hash <- hash_of file
            
            -- Append the filename to the hash. That's what 
            -- we'll be looking for on the stage.
            let file_item = hash ++ " " ++ file 
            
            -- Get a list of staged files.
            staged_files <- lines_from ".repo/stage"
            
            -- If the file is not on the stage, display a message.
            -- Otherwise, remove it.
            if not (file_item `is_in` staged_files)
                then display "That file is not staged for committing."
                else do

                    -- Delete the item from the list.
                    let new_staged_files = delete file_item staged_files
                    
                    -- Rewrite the stage file with the new list.
                    overwrite ".repo/stage" new_staged_files
                    
                    -- Display a message.
                    display ("-- " ++ file ++ " removed from the stage.")

-- Show any changed and/or updated files.
status [] = do

    -- Let's get a list of all files in the current directory.
    files <- files_in "."
    
    -- Display the current branch (found in the `current-point` file).
    current_branch <- readFile ".repo/current-point"
    display ("On branch " ++ current_branch)
    
    -- First, we want to list all staged files (listed in the `stage` file). 
    -- To begin, we'll read the lines of the `stage` file.
    staged_files <- lines_from ".repo/stage"
    
    -- Each line has a hash, a space, then the filename.
    -- We can drop the first 41 characters (the hash and space)
    -- to get just the filenames.
    let staged_filenames = (drop 41) `on_each_of` staged_files

    -- If there are any staged files, we'll display them. 
    if number_of staged_files > 0
        then do
            display "Files staged for committing:"
            display (one_string_of staged_filenames)
        else display "No files staged for committing.\n"
             
    -- Next, we want to find any files that have changed from the last 
    -- commit. We'll start by getting the last commit hash (in the  
    -- current branch file).
    latest_commit <- readFile (".repo/branches/" ++ current_branch)
        
    -- If no commits have been made yet, display untracked files.
    if null latest_commit
        then do
        
            -- Display a message.
            display "No commits yet."
            
            -- Go through each file and see if it's been backed up yet.
            on_all files (\file -> do

                -- Get a hash of the file.
                hash <- hash_of file

                -- What would it's path be if it were backed up already?
                backup_file <- path_of ".repo/files" hash
                
                -- Does it exist in the repo?
                in_repo_already <- doesFileExist backup_file

                -- Is it in the list of staged files?
                let staged_already = file `is_in` staged_filenames
                
                -- If it's not backed up, nor in the staged files, 
                -- display a message that says it's not tracked.
                when (not in_repo_already && not staged_already)
                     (display ("Untracked file: " ++ file)))
      
        -- Otherwise, we want to lookup the snapshot from the commit
        -- and see if any of the files have changed.
        else do

            -- Get the path to the latest commit file.
            commit_file <- path_of ".repo/commits/" latest_commit
            
            -- Read the commit file, and get the snapshot details.
            -- The snapshot will be on line 2.
            commit_details <- lines_from commit_file
            let snapshot_details = item (2 `out_of` commit_details)

            -- From the snapshot hash, we can derive the path to 
            -- the snapshot file. The hash will be everything 
            -- after "snapshot" plus a space.
            let snapshot_hash = drop 9 snapshot_details
            snapshot_file <- path_of ".repo/snapshots/" snapshot_hash

            -- Now we can read the snapshot file. 
            -- That will give us a list of hashes and filenames.
            -- The first 40 characters of each line will be the hash,
            -- the rest (after the space) will be the filename.
            snapshot_files <- lines_from snapshot_file
            let snapshot_hashes = (take 40) `on_each_of` snapshot_files
            let snapshot_filenames = (drop 41) `on_each_of` snapshot_files

            -- Next, we'll go through each file in the current directory, 
            -- and see if it's changed from what it was in the snapshot.
            on_all files (\file -> do

                -- Get a hash of the file.
                hash <- hash_of file

                -- Is the file already staged? 
                let staged_already = file `is_in` staged_filenames

                -- Is it in the snapshot? 
                let in_snapshot = file `is_in` snapshot_filenames

                -- Has it changed from the snapshot?
                -- We know it's changed if it's hash is different
                -- and so NOT in the list of snapshot hashes.
                let is_changed = not (hash `is_in` snapshot_hashes)

                -- If it's not staged, not in the snapshot, and 
                -- it's changed, display a message.
                when (not staged_already && in_snapshot && is_changed)
                     (display ("File has changed: " ++ file)))

            -- Display a blank line (for easier reading).
            display ""

            -- Finally, we'll go through each file in the current 
            -- directory, and we'll see if it's changed from what's 
            -- in the repo's backup (stored in .repo/files).
            on_all files (\file -> do

                -- Get a hash of the file.
                hash <- hash_of file

                -- Get the path if it were in the backup.
                backup_file <- path_of ".repo/files/" hash
                
                -- Does it exist in the repo?
                in_repo_already <- doesFileExist backup_file

                -- Is it in the list of staged files?
                let staged_already = file `is_in` staged_filenames

                -- Is it in the snapshot already? 
                let in_snapshot = file `is_in` snapshot_filenames

                -- If it's not backed up, nor in the staged files, 
                -- display a message that says so.
                when (not in_repo_already && 
                      not staged_already && 
                      not in_snapshot)
                     (display ("Untracked file: " ++ file)))
      
    -- Finish IO activity.
    finish_IO

-- Commit a snapshot to the repo.
commit [] = do
  
    -- STEP 1
    -- We first want to make sure that the stage includes files
    -- from the last commit which haven't changed. We'll start
    -- by getting a list of files in the current directory.
    files <- files_in "."
    
    -- We want to construct a list of hashes and filenames 
    -- for these files, and store it in a temporary snapshot file.
    let current_snapshot_file = ".repo/temp-current-snapshot"
    writeFile current_snapshot_file ""
    on_all files (\file -> do
        hash <- hash_of file
        let file_entry = (hash ++ " " ++ file ++ "\n")
        appendFile current_snapshot_file file_entry)
    
    -- Get all the lines from that file.
    current_files <- lines_from current_snapshot_file

    -- Clear the temporary snapshot file.
    removeFile current_snapshot_file 
    
    -- To get the last snapshot, we'll need to get the 
    -- latest commit from the current branch.
    current_branch <- readFile ".repo/current-point"
    latest_commit <- readFile (".repo/branches/" ++ current_branch)
    
    -- If there is no latest commit, then there is nothing to worry about.
    if null latest_commit
        then display "No previous commits."
                
        -- Otherwise, we want to look up all files in the last snapshot.
        else do
      
            -- Get the path to the commit file.
            commit_file <- path_of ".repo/commits/" latest_commit

            -- Read the commit file, and get the snapshot details.
            -- The snapshot will be on the second line, with 
            -- the hash coming after the word "snapshot" and a space.
            commit_details <- lines_from commit_file
            let snapshot_details = item (2 `out_of` commit_details)
            let snapshot_hash = drop 9 snapshot_details    

            -- Get the path to the snapshot fil.
            snapshot_file <- path_of ".repo/snapshots/" snapshot_hash
            
            -- Now we can read the file to get all the files listed there.
            snapshot_files <- lines_from snapshot_file

            -- If there are any snapshot files, we can find the 
            -- ones that haven't changed, and we can merge those
            -- with the list of staged files in the staged file.
            if number_of snapshot_files > 0 
                then do
              
                    -- Are there any files in both the snapshot
                    -- and the current directory?
                    let keep_files = snapshot_files `intersect` current_files

                    -- Merge that list with the staged files.
                    staged_files <- lines_from ".repo/stage"
                    let new_list = keep_files `merged_with` staged_files

                    -- Save that list in the stage file.
                    overwrite ".repo/stage" new_list
                    
                -- Otherwise, just display a message.
                else do
                    display "Nothing in the previous snapshot."
    
    -- Now we can read from the stage file and have a full list.
    staged_files <- lines_from ".repo/stage"

    -- STEP 2
    -- Next, we want to make a backup of each file, and
    -- we want to add its hash to a temporary "snapshot" file.
    let temp_file = ".repo/temp-snapshot"
    writeFile temp_file ""
    on_all staged_files (\file_entry -> do

        -- The stage list includes hashes folled by filenames.
        -- Keep the first 40 characters to get the hash,
        -- and drop the hash and space (41 characters) 
        -- to get just the filename.
        let hash = take 40 file_entry
        let original_file = drop 41 file_entry

        -- When we make a backup, we'll want to use the first
        -- two characters of the hash as a directory name, and
        -- the rest of the hash as a filename.
        let backup_directory = take 2 hash
        let backup_file = drop 2 hash
        let backup = ".repo/files/" ++ backup_directory ++ "/" ++ backup_file
        
        -- Create the directory.
        create_directory (".repo/files/" ++ backup_directory)

        -- Copy the file into the directory (if needed).
        backed_up_already <- doesFileExist backup
        if not backed_up_already
            then do
                copyFile original_file backup
                display  ("-- saving  " ++ hash ++ " <- " ++ original_file)
            else display ("-- the contents of " ++ original_file 
                           ++ " have already been backed up.")
        
        -- Now, add the hash and filename to the snapshot file.
        appendFile temp_file (hash ++ " " ++ original_file ++ "\n")

        -- Display a message on the screen.
        display ("-- logging " ++ hash ++ " " 
                               ++ original_file 
                               ++ " in snapshot."))

    -- STEP 3
    -- We want to hash the whole snapshot file.    
    -- We'll start by getting the hash of the temporary file.
    snapshot_hash <- hash_of temp_file
    
    -- We're going to copy the temp file into the snapshots 
    -- directory. The first two characters of its hash will 
    -- be the directory, and the rest will be its filename.
    let directory = take 2 snapshot_hash
    let filename = drop 2 snapshot_hash
    let new_filename = ".repo/snapshots/" ++ directory ++ "/" ++ filename
    
    -- Create the directory.
    create_directory (".repo/snapshots/" ++ directory)

    -- Copy the file into the directory (if needed).
    snapshot_already <- doesFileExist new_filename
    if not snapshot_already
        then do
            copyFile temp_file new_filename
            display ("Saving snapshot " ++ new_filename)
        else display ("Status hasn't changed since last commit.")

    -- Clear the temp file.
    removeFile temp_file

    -- STEP 4
    -- Now we need to create the commit file. To do that, we'll want
    -- to first gather the committer's name, email, and commit message.
    display "What is your name?"
    name <- getLine
    display "What is your email?"
    email <- getLine
    display "Enter a commit message:"
    message <- getLine
    
    -- Get the current branch from the `current-point` file.
    current_branch <- readFile ".repo/current-point"
    
    -- Get the last commit recorded in that branch's file.
    last_commit <- readFile (".repo/branches/" ++ current_branch)
    
    -- Put the relevant info together into some strings.
    let parent_info = if null last_commit 
                          then ("parent None\n")
                          else ("parent " ++ last_commit ++ "\n")
    let snapshot_info = "snapshot " ++ snapshot_hash ++ "\n"
    let personal_info = "name " ++ name ++ "\n" ++ 
                        "email " ++ email ++ "\n" ++ 
                        "message\n" ++ message
        
    -- Put all that info in a temp file.
    let commit_details = parent_info ++ snapshot_info ++ personal_info
    let temp_commit_file = ".repo/temp-commit"
    writeFile temp_commit_file commit_details
        
    -- Get a hash on that temp file.
    commit_hash <- hash_of temp_commit_file
    
    -- Copy the temp commit file into the commits directory. The first
    -- two characters will be the directory, the rest the filename.
    let commit_directory = take 2 commit_hash
    let commit_filename = drop 2 commit_hash
    let commit_path = (".repo/commits/" ++ commit_directory 
                                        ++ "/" 
                                        ++ commit_filename)
    
    -- Create the directory.
    create_directory (".repo/commits/" ++ commit_directory)

    -- Copy the file into the directory (if needed).
    commit_already <- doesFileExist commit_path
    if not commit_already
        then do
            copyFile temp_commit_file commit_path
            display ("Saving commit " ++ commit_path)
        else display ("Commit " ++ commit_path ++ " already done.")

    -- Remove the temp commit file.
    removeFile temp_commit_file
    
    -- Finally, we can clear the stage file.
    overwrite ".repo/stage" [""]
    
    -- Write the new commit to the branch file.
    overwrite (".repo/branches/" ++ current_branch) [commit_hash]
        
    -- Finish the IO activity.
    finish_IO

-- List all branches.
branches [] = do
  
    -- Get a list of all the files in the branches directory.
    files <- getDirectoryContents ".repo/branches"
    let branches = filter not_dot_or_doubledot files
        
    -- Figure out which branch we're on by reading
    -- the `current-point` file.
    current_branch <- readFile ".repo/current-point"
        
    -- Now display each one, 
    -- marking the current branch with an asterisk.
    on_all branches (\branch -> do
        if branch `is` current_branch 
            then display ("* " ++ branch)
            else display ("  " ++ branch))
    
    -- Finish the IO activity.
    finish_IO

-- Create a branch.
branch [branch_name] = do

    -- Find the last branch we were on. 
    -- It's listed in the current-point file.
    latest_branch <- readFile ".repo/current-point"
    
    -- Find the latest commit for that branch.
    -- It's listed in the branch's file.
    latest_commit <- readFile (".repo/branches/" ++ latest_branch)

    -- Create a new branch file, with the latest commit.
    writeFile (".repo/branches/" ++ branch_name) latest_commit
    
    -- Display a message
    display ("Created branch: " ++ branch_name)
    
    -- Finish the IO activity.
    finish_IO

-- Checkout a branch.
checkout [branch] = do

    -- STEP 1
    -- First we want to get the snapshot for this branch.
    -- We'll start by getting the current commit.
    current_branch <- readFile ".repo/current-point"
    current_commit_hash <- readFile (".repo/branches/" ++ current_branch)

    -- Get the path to that commit file.
    current_commit_file <- path_of ".repo/commits/" current_commit_hash
    
    -- Now we can read the commit file and get the snapshot hash. The
    -- snapshot is the second line, and the hash occurs after the 
    -- word "snapshot" and a space (the first 9 characters).
    current_commit_details <- lines_from current_commit_file
    let current_snapshot_details = item (2 `out_of` current_commit_details)
    let current_snapshot_hash = drop 9 current_snapshot_details

    -- With that, we can get the path to the snapshot file.
    current_snapshot_file <- path_of ".repo/snapshots/" current_snapshot_hash

    -- Now we can get all the lines in the snapshot file.
    current_snapshot_files <- lines_from current_snapshot_file
    
    -- STEP 2
    -- Next, we want to get the snapshot for the destination branch.  
    -- We'll start by getting the last commit recorded for it.
    dest_commit_hash <- readFile (".repo/branches/" ++ branch)
    
    -- From that, we can figure out the path to the commit file.
    dest_commit_file <- path_of ".repo/commits/" dest_commit_hash                                   
    -- Read the corresponding commit file and get the snapshot hash.
    -- The snapshot is the second line, and the hash occurs after
    -- the word "snapshot" and a space (the first 9 characters).
    dest_commit_details <- lines_from dest_commit_file
    let dest_snapshot_details = item (2 `out_of` dest_commit_details)
    let dest_snapshot_hash = drop 9 dest_snapshot_details
        
    -- Now we can figure out the path to the snapshot file. 
    dest_snapshot_file <- path_of ".repo/snapshots/" dest_snapshot_hash
    
    -- Get the list of files from the snapshot file 
    dest_snapshot_files <- lines_from dest_snapshot_file
        
    -- Now that we have a list of all the files in the current snapshot
    -- and the destination snapshot, we can restore the destination 
    -- snapshot. To do that, we'll first delete each file that is in 
    -- the current snapshot but not the destination snapshot.    
    on_all current_snapshot_files (\item -> do

        -- Get the hash and filename for this item. The hash will be 
        -- the first 40 characters, the filename will be everything
        -- after the hash and a space (41 characters).
        let hash = take 40 item
        let filename = drop 41 item

        -- Is this item in the destination snapshot?
        let in_destination = item `is_in` dest_snapshot_files

        -- if not, we'll delete it.
        when (not in_destination) (removeFile filename))

        
    -- Now we can restore any files in the destination snapshot
    -- that are not present. 
    on_all dest_snapshot_files (\item -> do

        -- Get the hash and filename for this item. The hash will be 
        -- the first 40 characters, the filename will be everything
        -- after the hash and a space (41 characters).
        let hash = take 40 item
        let filename = drop 41 item
        
        -- Calculate the path to the backed up file. 
        backup <- path_of ".repo/files/" hash
        
        -- Does the item exist?
        exists_already <- doesFileExist filename

        -- If not, copy its backup to the current location.
        when (not exists_already) (copyFile backup ("./" ++ filename)))
    
    -- Rewrite current-point to the new branch.
    writeFile ".repo/current-point" branch
    
    -- Display a message.
    display ("Switched to branch: " ++ branch)
    
    -- Finish IO activity.
    finish_IO
            
-- Merge another branch into the current one.
merge_in [branch] = do

    -- Get the latest commit for the current branch.
    current_branch <- readFile ".repo/current-point"
    current_commit <- readFile (".repo/branches/" ++ current_branch)
    
    -- Get the latest commit for the merge branch.
    merge_commit <- readFile (".repo/branches/" ++ branch)
    
    -- Is the current branch a parent of the merge branch?
    is_parent <- current_commit `is_parent_of` merge_commit

    -- If it is a parent, we can fast-forward the current
    -- branch to the merge branch.
    if is_parent
        then do
        
            -- Display a message
            display ("Fast forwarding " ++ current_branch 
                                        ++ " to " 
                                        ++ merge_commit)

            -- Change the commit in the branch file to 
            -- the merge commit.
            removeFile (".repo/branches/" ++ current_branch)
            writeFile (".repo/branches/" ++ current_branch) merge_commit
            
            -- Now checkout the branch again. 
            checkout [current_branch]
        
        -- If it is not a parent, and so cannot be fast-forwarded,
        -- then we can ask the user to do the merge manually.
        else do 
            display "Cannot merge by fast-forwarding."
            display "It would be best if you merge manually,"
            display "then recommit."


-- -----------------------------------------------------
    
-- MERGE UTILITIES
    
-- -----------------------------------------------------    

-- Find out if one commit is a parent of another.
commit1 `is_parent_of` commit2 = do

    -- Get the commit file for commit2. The directory will be the 
    -- first 2 characters, and the filename will be the rest.
    let directory = take 2 commit2
    let filename = drop 2 commit2
    let file = ".repo/commits/" ++ directory ++ "/" ++ filename
    
    -- Get the parent commit hash from that file. It will be on the
    -- first line, after the word "parent" and a space (7 characters).
    commit <- readFile file
    let commit_details = break_into_lines commit
    let parent_details = item (1 `out_of` commit_details)
    let parent_hash = drop 7 parent_details
        
    -- Now that we have the parent hash, we can compare it 
    -- against commit1. If the parent hash is "None", then 
    -- it has no parents, so commit1 cannot be a parent of it.
    if parent_hash `is` "None"
        then return (False)
        else do
      
            -- If the parent hash of commit2 is commit1, then 
            -- of course commit1 is the direct parent of commit2.
            if parent_hash `is` commit1
                then return (True)
                else do
              
                    -- Otherwise, we need to run this check again.
                    do_again <- commit1 `is_parent_of` parent_hash
                    return (do_again)



-- -----------------------------------------------------
    
-- UTILITIES FOR LOOKING UP HASHES
    
-- -----------------------------------------------------    

-- Get the snapshot of a commit.
snapshot_of commit_hash = do

    -- From the commit hash, we can figure out the path to the
    -- commit file. The first two characters of the hash will be 
    -- the directory. The rest will be the filename.
    let directory = take 2 commit_hash
    let filename = drop 2 commit_hash
    let file = ".repo/commits/" ++ directory ++ "/" ++ filename
        
    -- Read the file, and break it into lines.
    commit <- readFile file
    let commit_details = break_into_lines commit
        
    -- Now we can get the snapshot details. The snapshot's hash
    -- will be on the second line, after the word "snapshot"
    -- and a space (9 characters).
    let snapshot_details = item (2 `out_of` commit_details)
    let snapshot_hash = drop 9 snapshot_details
        
    -- With the snapshot hash, we can figure out the snapshot file.
    let snapshot_directory = take 2 snapshot_hash
    let snapshot_filename = drop 2 snapshot_hash
    let snapshot_file = ".repo/snapshots/" ++ snapshot_directory
                                           ++ "/"
                                           ++ snapshot_filename
    
    -- Read the file, and break it into lines.
    snapshot <- readFile snapshot_file
    let snapshot_details = break_into_lines snapshot
    return (snapshot_details)

-- Get the relative path to a file, given its SHA1 hash.
-- E.g., from "22596363b3de40b06f981fb85d82312e8c0ed511"
-- return "22/596363b3de40b06f981fb85d82312e8c0ed511".
pathify hash = do 
  
    -- The first two characters are the directory name.
    let directory = take 2 hash
    
    -- The rest is the filename. Drop the first two,
    -- characters, then take 38 characters only (we don't
    -- need any extra spaces or line breaks at the end).
    let filename = take 38 (drop 2 hash)
        
    -- Construct the path and use it.
    let path = directory ++ "/" ++ filename
    
    return (path)

-- Construct the path to a repo file. The prefix is the 
-- location in the repo, e.g., ".repo/files/", and the
-- hash is the hash of the file.
path_of prefix hash = do
    relative_path <- pathify hash
    return (prefix ++ relative_path)

-- Read a file and break it into lines.
lines_from file = do
    contents <- readFile file
    return (break_into_lines contents)

-- Remove and rewrite a string of lines to a file. 
overwrite file new_lines = do
    removeFile file
    writeFile file (strip (one_string_of new_lines))


-- -----------------------------------------------------
    
-- HASH UTILITIES
    
-- -----------------------------------------------------    
    
-- Get the SHA1 hash of a file's contents.
hash_of :: FilePath -> IO String
hash_of file = do
    contents <- bytes_of file
    let hashed_bytes = hashify contents
    let hash = hexify hashed_bytes
    return (hash)

-- Read the bytes of a file
bytes_of file = Lazy.readFile file

-- Hash a file's contents (more exactly, hash its bytes).
hashify bytestream = Hash.hashlazy bytestream

-- Convert a stream of bytes to a hexadecimal alphanumeric string.
hexify bytestream = Strict.unpack bytestream >>= printf "%02x"    



-- -----------------------------------------------------
    
-- MISC UTILITIES
    
-- -----------------------------------------------------
    
-- Make `first_of` an alias for `head`.
first_of xs = head xs

-- Make `rest_of` an alias for `tail`.
rest_of xs = tail xs

-- Make `number_of` an alias of `length`
number_of xs = length xs

-- Make `use` and `item` a way of specifying a value.
use x = x
item x = x

-- Make `break_into_lines` an alias for `lines`
break_into_lines string = lines string

-- Make `one_string_of` an alias for `unlines`
one_string_of list_of_lines = unlines list_of_lines

-- Make `is_in` an alias for `elem`.
x `is_in` list = x `elem` list

-- Make `not_in` an alias for not x `notElem` y.
x `not_in` list = x `notElem` list

-- Make `out_of n xs` an alias for `list !! n` (which gets item n in list).
-- It starts counting at 1 rather than 0 though, and it is meant to be 
-- written with infix notation. So, 1 `outof` list returns the first item, 
-- 2 `outof` list returns the second item, and so on.
n `out_of` xs = (xs !! (n - 1))

-- Make `exit` an alias for `Exit with success, status code 0`.
exit = exitWith ExitSuccess

-- Make `die` an alias for `Exit with a fail, status code 1`.
die = exitWith (ExitFailure 1)

-- Print a message to standard out.
display message = putStrLn message

-- Make `unrecognized` an alias for `isNothing`.
unrecognized item = isNothing item

-- Run a command with arguments.
run_with command arguments = do
    let (Just run_function) = command
    run_function arguments
    
-- Get the files only in a directory (ignore sub-directories).
files_in directory = do
    all_items <- (getDirectoryContents directory)
    filterM doesFileExist all_items
    
-- Make `on_all` an alias for `forM`.
on_all = forM

-- Make `on_each_of` an alias for `map`.
x `on_each_of` y = map x y

-- Make `filtered_by` an alias for `filter`.
list `filtered_by` function = filter function list

-- Make `create_directory` an alias for `createDirectoryIfMissing`
create_directory directory = createDirectoryIfMissing False directory

-- Filter out the "." and ".." from a list.
not_dot_or_doubledot x = not (x `is_in` [".", ".."])

-- Perform an empty IO action.
finish_IO = return ()

-- Make `is` an alias for `==`.
x `is` y = (x == y)

-- Make `merged_with` an alias for `union`.
list1 `merged_with` list2 = list1 `union` list2



