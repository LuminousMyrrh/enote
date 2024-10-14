# enote

**enote** is an Emacs package designed to manage notes associated with specific lines in your code files. It allows users to create, read, delete, and navigate through notes seamlessly within Emacs.
![image](https://github.com/user-attachments/assets/4f6ea330-7de1-41d0-9608-70bc4353d054)


## Features

- Create notes attached to specific lines in your code files.
- View existing notes in a dedicated frame.
- Easily remove notes when they are no longer needed.
- Jump to the next or previous note directly from your code.
- Customize the appearance of the note frame with background and foreground colors.

## Installation

1. Clone the repository:
  ```bash
  git clone https://github.com/LuminousMyrrh/enote/
  ```
2. Add 'enote.el' to your Emacs load path.
3. Require the package in your Emacs configuration:
  ```elisp
  (require 'enote)
  ```
    
## Key Bindings
   - C-c c: Toggle enote.
   - C-c g: Delete a selected note.
   - C-c h: Jump to the next note.
   - C-c l: Jump to the previous note.

## Usage
   - To create a new note, navigate to the line you want to annotate and use C-c c.
   - Enter your note content in the opened frame and save it.
   - Use C-c h and C-c l to navigate through your notes.
