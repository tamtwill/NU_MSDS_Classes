# Database Systems and Data Prep

In this class, the instructor required all work to be done using Python notebooks.  There were 4 projects in the class.  Each focued on a separate aspect of the material.

## Course Description
This course is about manipulating data to prepare it for analysis using Python tools. SQL and NoSQL technologies are referred to, to some extent and in basic way. It’s expected that ElasticSearch will be the NoSQL store that we’ll be using to provide access to some assignment data this term.
The data used for assignments varies in structure and volume. It includes poorly structured and user- generated content data. The data are stored in various formats, and are about things like customer transactions, air transportation, corporate email, and customer evaluations of hospitality experiences.

In this course students are required to “peer review” their classmates’ contributions to the course. Students’ peer reviews count towards their final grade in the course.

## Course Perspective and Pedagogical Philosophy
This isn’t a course about Python, SQL, or NoSQL, per se. It’s about getting data into a desired condition to be analyzed. It’s about formulating, implementing, and testing solutions to the problem of getting data from a current state to a required state. So, code, datasets, and storage technologies aside, it’s about practical data preparation problem-solving.

In the opinion of many, a good way to learn many things is by doing them. The famous mathematician and educator Paul Halmos once said that to do is to know, and to talk is not to teach. I’ll add to this that a book is not by itself a course. There are readings in this course, as there are in other MSPA courses. The readings provide some reference material and can fill in content gaps with respect to the course learning objectives. For some tasks in this course students may need to go beyond the required and recommended readings by looking elsewhere, like online. This is a key aspect of practical problem-solving, a critical skill for data scientists and for predictive analytics professionals.

In this course the problems to be solved consist of getting data from “State A” to “State B.” They often need to be further define these problems so as to open up potential solutions, and ways to apply solutions using (usually) Python code that provides results. Students in this course are not provided with the code to do the assignments, or with complete solutions to them. General feedback, tips, and “hints” on assignments are provided, by me (your instructor), our TA, and also by your fellow 420 students as participants in this course’s learning community.

## Course Learning Goals
These are the common 420 course goals. They are achieved by completing the course readings and doing the assignments. You will be the ultimate, and most important, judge of whether you attained them.

+ Define key terms, concepts and issues in data management and database management systems with respect to predictive modeling
+ Evaluate the constraints, limitations and structure of data through data cleansing, preparation and exploratory analysis to create an analytical database.
+ Use object-oriented scripting software for data preparation.

### Graded Exercise 1: 
1. Read each data file into a Pandas DataFrame. Add meaningful names (i.e., names that would make sense to other people, given the data) to the columns of each DataFrame.
Provide your syntactically correct, commented code.
Print the first three rows of each DataFrame. Provide your code, and the results it produced.
2. Check each DataFrame for duplicate records. For each, report the number of duplicates you found.
Provide your commented, syntactically correct code and the results it produced.
3. Describe the data types of the columns in each of the DataFrames.
Provide your commented, syntactically correct code and the results it produced.
4. Determine how many of the airlines are "defunct."
Provide your definition of what a defunct airline is.
Provide your commented, syntactically correct code and the results it produced.
5. Determine how many "routes from nowhere" there are in the data. These are flights that don't originate from an airport.
Provide your commented, syntactically correct code and the results it produced.
6. Save your DataFrames for future use. You may pickle them, put them in a shelve db, on in a the tables of a SQL db. Check to make sure that they are saved correctly.


### Graded Exercise 2:  
Provide the above in up to six (6) pages, but in no more than six pages, in a pdf file. Be sure that everything is readable. Address each of the seven (7) items in turn. Do 1 by providing your commented code and results, Then do 2 providing code + results, and so on. Do not provide a list of code for all of the above items in a block, followed by the results of your code in a block. An assignment organized in this way will be returned ungraded. Be sure all of your code is syntactically correct, and that it approximates good Python coding style.

### Graded Exercise 3:
In no more than six(6) pages, in a pdf file, provide the following.
Part I
Provide your commented, syntactically correct code for each step you took to create the two results required.
For each step, provide example output that demonstrates results. Your commented, syntactically correct code and your examples of the results of applying it should provde a full explanation of how each step in your process. Do not submit all of your code for this assigment in a single block, followed by a block of all of your output. Assignments organized in this manner will not be graded.
Ways to demonstrate the results of creating or transforming data include outputting example records from DataFrames (using the .head() method, for example), and describing the types of a DataFrame's columns.
The above should give you an idea of what your assignment should be like. Review the above for details of what you deliverables should include. Don't forget to consider the stated objectives.
Part II
This should be done like part I, above. Take note of the requirement that with your code should work for an arbitrarily large number of json files. Also, assume that you don't know how many files it may need to work on, a priori.

### Graded Exercise 4:
In six (6) pages or less in a pdf file, but in no more than six pages, address each of the items, above. Include your code (documented and syntactically correct, of course), and any results indicated. Make sure your content is readable. Don't forget to organize by providng for each part both code and results. For example, do 1. by providing your code and then the requested results, then do 2. with code and results, and so on. Do not first provide all of your code, and after all of the required results.
Be prepared to share your code and results.