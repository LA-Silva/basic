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

  if counter < 3 goto loop_start

print "Loop finished."

' Test String Functions
print ""
print "--- Testing String Functions ---"
s$ = "HELLO WORLD"
print "Original: " + s$
print "Length:"
print LEN(s$)
print "LEFT$(s$, 5):"
print LEFT$(s$, 5)
print "RIGHT$(s$, 5):"
print RIGHT$(s$, 5)
print "MID$(s$, 7, 5):"
print MID$(s$, 7, 5)

' Test INPUT
print ""
print "--- Testing INPUT ---"
print "Please enter your name:"
input user_name
print "Hello, "
print user_name

' Test GOSUB
gosub somelabel


' Test EXIT
print ""
print "--- Testing EXIT ---"
print "This will be the last line."
exit
print "This should not be printed."

somelabel:
print "gosub test"
print "hello again" + user_name
return