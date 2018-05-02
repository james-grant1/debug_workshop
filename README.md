# debug_workshop
Repository for the Debugging Numerical Software - Bath 4-5 Jun 2018

This repository is for bugs, and material developed as part of the Workshop on Debugging Numerical Software.

## Submitting a bug
If you are submitting a bug for the workshop please proceed as follows:
1. **Fork the repository** ([more on forking](https://help.github.com/articles/fork-a-repo/))
2. **Create a folder** with your name.
3. **Add a file `readme.txt`** to this directory. This file should contain:
   - Your full name
   - The title of the bug (no more than one line)
4. **Add the source code to a subdirectory `src`** of this folder. Ideally the `src` subdirectory should contain a fully functional example which can be compiled and run during the workshop.
5. **Include two separate `.tex` files in a subdirectory `tex`** of your subdirectory. The content of each should fit on a single slide:
   - The slide in the **first file** should include a brief **overview of the bug**, which we will ask you to introduce at the start of the Bug Hunt, and include context for the code and bug, language, issue.
   - The slide in the **second file** should **summarise the solution** e.g. how the bug was resolved (if it was), what methods were used, how long did it take and what was the impact of the bug. This slide will only be revealed at the end of the bug hunt and you should not make the solution available to the other participants in your group.
6. Once you added the source code and slides, **create a pull request** on github ([more on pull requests](https://help.github.com/articles/about-pull-requests/)) to get the changes back into the main repository. We will merge these into the main branch so that they will be available to attendees at the workshop and collate the tex into a single document for the introductions.

If it is not practical to upload the code to the reposity, or the software is not open source, please contact the organisers, r.j.grant@bath.ac.uk or e.mueller@bath.ac.uk to discuss how best to include your example in the workshop.
