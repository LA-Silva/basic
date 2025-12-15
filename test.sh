#!/bin/bash

# Test script for the PHP BASIC interpreter.
# This script will:
# 1. Create a `test.basic` file.
# 2. Define the expected output of running the interpreter on that file.
# 3. Run the interpreter, piping in a name for the INPUT statement.
# 4. Compare the actual output with the expected output.
# 5. Report success or failure.

set -e

# Create the test BASIC file
cat > test.basic << 'EOF'
' This is a test program for the BASIC interpreter

' Test printing strings and numbers
print "--- Testing PRINT ---"
print "Hello, World!"
print 12345

' Test variables and assignments
print ""
print "--- Testing Variables and Expressions ---"
x = 10
y = 20
print "x is"
print x
print "y is"
print y

z = x + y
print "x + y is"
print z

a = z * 2
print "z * 2 is"
print a

b = a / 4
print "a / 4 is"
print b

c = b - 5
print "b - 5 is"
print c

' Test conditional logic and loops
print ""
print "--- Testing IF/GOTO ---"
counter = 0
loop_start:
  print "Counter is"
  print counter
  counter = counter + 1
  if counter < 3 loop_start

print "Loop finished."

' Test INPUT
print ""
print "--- Testing INPUT ---"
print "Please enter your name:"
input user_name
print "Hello, "
print user_name

' Test GOSUB/RETURN
print ""
print "--- Testing GOSUB/RETURN ---"
gosub my_subroutine
print "Returned from subroutine."

' Test EXIT
print ""
print "--- Testing EXIT ---"
print "This will be the last line."
exit
print "This should not be printed."

my_subroutine:
  print "Inside the subroutine!"
  return

print "This should also not be printed."
EOF

# Define the expected output
read -r -d '' EXPECTED_OUTPUT <<'EOF'
--- Testing PRINT ---
Hello, World!
12345

--- Testing Variables and Expressions ---
x is
10
y is
20
x + y is
30
z * 2 is
60
a / 4 is
15
b - 5 is
10

--- Testing IF/GOTO ---
Counter is
0
Counter is
1
Counter is
2
Loop finished.

--- Testing INPUT ---
Please enter your name:
Hello, 
Tester

--- Testing GOSUB/RETURN ---
Inside the subroutine!
Returned from subroutine.

--- Testing EXIT ---
This will be the last line.
EOF

# Run the interpreter and capture the output.
# We pipe "Tester" into the script to handle the `input` command.
echo "Running test..."
ACTUAL_OUTPUT=$(echo "Tester" | php basic.php test.basic)

# Compare the actual output to the expected output
if [ "$ACTUAL_OUTPUT" == "$EXPECTED_OUTPUT" ]; then
    echo -e "\033[0;32m✅ All tests passed!\033[0m"
    rm test.basic
    exit 0
else
    echo -e "\033[0;31m❌ Test failed!\033[0m"
    echo "--- EXPECTED OUTPUT ---"
    echo "$EXPECTED_OUTPUT"
    echo "--- ACTUAL OUTPUT ---"
    echo "$ACTUAL_OUTPUT"
    echo "-----------------------"
    rm test.basic
    exit 1
fi