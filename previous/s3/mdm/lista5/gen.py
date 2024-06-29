


template = "\\node at ({xp},{yp})"
template2 = " "

for y in range(5):
    for x in range(7):
        st = template.format(xp=(-1.5 + x), yp=(2.5 - y))
        res = st + " {" + str((x+y)%7) + "};"
        print(res)

        