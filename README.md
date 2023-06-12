# Haskell Student Directory

Haskell Student Directory *or HSD* is a directory management program designed to demonstrate the application of functional programming techniques in the context of the university administration.

## Features

HSD allows you to:

* Manage the personal information of students, modules and lecturers
* Manage student enrollment in programme modules
* Generate and export markdown reports 

## System Requirements

To build and run this project, you will need the following tools and dependencies:

* Stack 2.93 or later
* GHC   9.2.7 or later
* Cabal 3.8.1 or later


## Building and Running HSD

To build the project, run the following command from a development shell.
```sh
stack build
```

To run the project, it is advised to copy the executable from ```.stack-work```; however, you can also run from a development shell using the following command.

```sh
stack exec haskell-student-directory-exe
```

## Using HSD

### **Global Options**

When running HSD for any command, you may provide an optional argument ```--path``` to specify the directory on which to operate. By default the program will use the current working directory.

You can also run HSD using a ```--help``` flag to see a list of available commands.

### **Initialising a new Directory**

To initialise a new directory, run the following command.

```sh
./haskell-student-directory-exe initialise
```

This will create all the necessary data files and allow you to begin registering entities.

If you wish to clear the current database, you can overwrite existing files with the ```--overwrite``` option, as shown below.

```sh
./haskell-student-directory-exe initialise --overwrite
```

### **Registering and Deleting Entities**

To register a new entity, you can use the following command syntax. Where *ENTITY* can be either student, module or lecture.

```sh
./haskell-student-directory-exe add ENTITY [FIELDS]
```

**Examples**:

```sh
# Adding a student
# Required Fields: identifier, first name, last name, email
./haskell-student-directory-exe add student 2020859 John Smith j.smith@example.com
```

```sh
# Adding a lecturer
# Required Fields: identifier, title, first name, last name, email
./haskell-student-directory-exe add lecturer 2018764 Professor Glyn Normington g.normington@example.com
```

```sh
# Adding a module
# Required Fields: identifier, name, department, lecturer identifier
./haskell-student-directory-exe add module 2220 "Functional Programming" "Digital Technologies" 2018764
```

Entities can also be deleted using the following command syntax.

```sh
./haskell-student-directory-exe delete ENTITY IDENTIFIER
```

**Example:**

```sh
# Delete a student
./haskell-student-directory-exe delete student 2020859
```

### **Reteriving a Single Entity**

Once an entity has been added to the directory, it can be shown using the following command syntax. Where *IDENTIFIER* matches the required student, module or lecturer.

Running the show command on a student or module will also yield a list of enrolled modules or students as appropriate. 

```sh
./haskell-student-directory-exe show ENTITY IDENTIFIER
```

**Examples**:

```sh
# Retrieving a student
./haskell-student-directory-exe show student 1023476

[Student] (Identifier: 1023476) (Name: John Smith) (Email: john.smith@winchester.ac.uk)
```

```sh
# Retrieving a lecturer
./haskell-student-directory-exe show lecturer 2018764

[Lecturer] (Identifier: 2018764) (Name: Professor Glyn Normington) (Email: g.normington@winchester.ac.uk)
```

```sh
# Retrieving a module
./haskell-student-directory-exe show module 2220

[Module] (Identifier: 2220) (Name: Functional Programming) (Department: Digital Technologies) (Lecturer: 2018764)
```

### **Searching Records**

Entities can also be retrieved through the search command. The syntax for this is shown below.

```sh
./haskell-student-directory-exe search ENTITY QUERY
```

This will match the given query to any record containing the string using a regular expression. Similar to the behaviour of ```LIKE %STRING%``` in SQL.

**Examples:**

```sh
# Searching for students matching 'john'
./haskell-student-directory-exe search student "john"

[Student] (Identifier: 1023476) (Name: John Smith) (Email: john.smith@winchester.ac.uk)
[Student] (Identifier: 7142095) (Name: Michael Johnson) (Email: michael.johnson@winchester.ac.uk)
```

```sh
# Searching for modules matching 'Introduction'
./haskell-student-directory-exe search module "Introduction"

[Module] (Identifier: 1234) (Name: Introduction to Computer Science) (Department: Science) (Lecturer: 5278913)
[Module] (Identifier: 3456) (Name: Introduction to Business) (Department: Business and Law) (Lecturer: 2051689)
```
### **Updating Entities**

Entities can also be updated using syntax identical to the ```add``` command. Fields marked with a ```dash``` will be ignored, and their original value will be preserved.

```sh
./haskell-student-directory-exe update ENTITY IDENTIFIER [FIELDS]
```

**Example:**

```sh
# Updates the surname and email of student 'John Smith' to 'John Roberts'
./haskell-student-directory-exe update student 1023476 - Smith j.roberts@winchester.ac.uk

# Before
[Student] (Identifier: 1023476) (Name: John Smith) (Email: john.smith@winchester.ac.uk)

# After
[Student] (Identifier: 1023476) (Name: John Roberts) (Email: john.roberts@winchester.ac.uk)
```

### **Generating Reports**
Reports containning a list of enrolled modules and students can be generated and exported using the command syntax below. 

```sh
./haskell-student-directory-exe export ENTITY IDENTIFIER --output PATH
```

*Note: ```--output``` is an optional argument*

**Examples:**

```sh
# Generating a student report, exporting to documents
./haskell-student-directory-exe export student 1023476 --output="~/Documents/"
```

```sh
# Generating a module report
./haskell-student-directory-exe export module 2220."
```

## Running Tests

Unit and Integration tests can be from a development shell using the following command.

```sh
stack test
```

## Code Formatting

Code for this project was formatted using **Stylish Haskell** V0.14.3. More information on can found on [Github](https://github.com/haskell/stylish-haskell)
