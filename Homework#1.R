# ==========================================
# Homework 1 - Problem 1 Setup & Code
# ==========================================

# Setup code provided in the assignment
student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section <- c("A", "B", "A", "B", "A", "B")
quiz1 <- c(82, 91, 76, 88, 95, 69)
quiz2 <- c(85, 89, 80, 92, 94, 74)
passed <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)

# ------------------------------------------
# Part A: Building Data Structures
# ------------------------------------------

# 1. Convert section to a factor with levels in the order "A", "B"
section <- factor(section, levels = c("A", "B"))

# 2. 
students <- data.frame(student_id, section, quiz1, quiz2, passed)

# 3. 
score_matrix <- matrix(c(quiz1, quiz2), nrow = 6, ncol = 2)
rownames(score_matrix) <- student_id
colnames(score_matrix) <- c("quiz1", "quiz2")

# 4. Create course_record list
course_record <- list(
  course = "R Programming",
  scores = students,
  cutoffs = c(pass = 70, excellent = 90)
)

# Inspecting the objects
typeof(section); class(section)
str(students); dim(students)
typeof(score_matrix); dim(score_matrix)
class(course_record); length(course_record)

# [Written Answer]
# A vector is a 1D sequence of the same data type, while a factor is a vector used to handle categorized data with predefined levels. 
# A matrix is a 2D grid where all data must be the same type, whereas a data frame is a 2D table that allows columns of different data types. 
# A list is the most flexible structure, capable of holding collections of entirely different objects and sizes together.

# ------------------------------------------
# Part B: Subsetting
# ------------------------------------------

# 1. Extract student S04’s second quiz score from score_matrix
s04_q2 <- score_matrix["S04", "quiz2"]

# 2. Extract first two matrix rows while preserving matrix dimensions
first_two_rows <- score_matrix[1:2, , drop = FALSE]

# 3. Extract course from course_record three different ways
extract_1 <- course_record["course"]
extract_2 <- course_record[["course"]]
extract_3 <- course_record\$course

# [Written Answer]
# The single bracket `[` extracts a sublist containing the element, keeping the original list structure. 
# The double bracket `[[` and the dollar sign `$` both drill down into the list to extract the actual raw value directly, with `$` allowing you to select elements by name without quotes.

# ------------------------------------------
# Part C: Vectorized Calculations
# ------------------------------------------

# 1. Add average (row-wise mean of the two quizzes) to students
students\$average <- (studentsquiz1 + studentsquiz2) / 2

# 2. Add excellent (TRUE when average >= 90)
studentsexcellent <- studentsaverage >= 90

# 3 & 4. Select section A students whose average is at least 80, keeping specific columns
sec_A_high <- students[students\(section == "A" & students\)average >= 80, c("student_id", "section", "average")]

# 5. Create a named numeric vector of all student averages using student IDs as names
student_averages <- students\$average
names(student_averages) <- students\$student_id

# [Written Answer]
# These calculations are vectorized because R automatically applies math operations and logical comparisons to every element in the vector simultaneously. 
# This eliminates the need to write manual loops to look through rows one by one.

