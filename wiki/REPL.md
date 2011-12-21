
    s = "pry"

    cd String

    show-doc s.each_line
    show-doc String#each_line
    show-doc each_line

    show-method s.each_line
    show-method String#each_line
    gist-method each_line

Directory traversal

    pry(main)> cd A
    pry(A)> cd B
    pry(B)> cd C
    pry(C)> pwd
    --
    0: main
    1: A
    2: B
    3* C

    pry(C)> cd 1
    pry(A) pwd
    --
    0: main
    1* A
    2: B
    3: C

X
