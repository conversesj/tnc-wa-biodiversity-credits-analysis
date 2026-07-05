GitHub Repository Branch Protocol

1. **Purpose:** This protocol shows how to use GitHub branches to manage scripts and document updates. A branch can create a separate working copy of the repository and avoid editing *main* directly. (Use a branch when you are updating data, editing scripts, cleaning files, adding documentation, or fixing a bug. Usually, the rule is no major data, script, or documentation update should be pushed directly to main. Create an issue, work on a branch, open a pull request, and merge only after review.)
2. **Steps**

   1. Clone the repository to your local environment and make necessary edits

      2. git clone https://github.com/JacW27/tnc-wa-biodiversity-credits-analysis.git
      3. cd tnc-wa-biodiversity-credits-analysis
   2. Before creating a new branch, always update your local *main* branch first

      1. git switch main
      2. git pull origin main
      3. Create a new branch: git switch -c branch\_name
      4. (check which branch you're on: git branch)
   3. Add file(s)

      1. git add path/to/file1 (path/to/file2)
      2. (Add all changed files: git add .)
   4. Commit the changes

      1. git commit -m "write a short description of your changes"
   5. Push the branch back to GitHub

      1. git push -u origin branch\_name
      2. (After the first push, you can directly use: git push)
   6. Open a Pull Request on GitHub

      1. Go to your repository on GitHub.
      2. Click "Compare \& pull request".
      3. Make sure: base = main, compare = your-branch-name.
      4. Write a clear title and summary of your changes.
      5. Click Create pull request.
   7. Go back to the repository and review the changed files. After review:

      1. Merge pull request
      2. git switch main
      3. git pull origin main
3. **Common bugs and solutions**

   1. Merge conflict

      1. It happens when two people edit the same part of the same file. You may see a message, like

         1. <<<<<<< HEAD your version ======= other person's version >>>>>>> branch-name
      2. Solution:

         1. Open the conflicted file.
         2. Decide which version to keep, or combine both.
         3. Delete the conflict markers.
         4. Save the file.
         5. Run:

            1. git add path/to/conflicted-file
            2. git commit -m "Resolve merge conflict"
            3. git push
   2. Accidentally working on main

      1. Usually it's fine. No need for panic.
      2. Solution:

         1. Check your current branch:

            1. git branch
         2. If you made changes but have not committed yet, create a new branch and keep the changes

            1. git switch -c new-branch-name
         3. Then commit and push from that branch.
   3. Accidentally added the wrong/unwanted file

      1. If you added a file but have not committed yet:

         1. git restore --staged path/to/file
      2. If you want to remove all staged files:

         1. git restore --staged .
      3. Then check:

         1. git status
   4. Accidentally committed the wrong file

      1. If you committed but have not pushed yet, you can undo the last commit while keeping your changes:

         1. git reset --soft HEAD\~1
      2. Then unstage the wrong file:

         1. git restore --staged path/to/wrong-file
      3. Commit again:

         1. git commit -m "Correct commit message"
4. **Optional**

   1. See detailed changes: git diff
   2. After editing, check what changed: git status
   3. Check where you are: pwd



