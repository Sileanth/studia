#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

void print_uid(void)
{
    uid_t ruid, euid, suid;
    getresuid(&ruid, &euid, &suid);
    printf("ruid: %d, euid: %d, suid: %d\n", ruid, euid, suid);
}

int main(int argc, char* argv[])
{
    if (geteuid()) {
        printf("Must be run with sudo");
        return 1;
    }
    if (argc < 2) {
        printf("Argument not supplied");
        return 1;
    }
    setreuid(1000, -1);
    print_uid();
    switch (atoi(argv[1])) {
    case 0:
        printf("setuid(2000)\n");
        setuid(2000);
        print_uid();
        break;
    case 1:
        printf("setreuid(-1, 2000)\n");
        setreuid(-1, 2000);
        print_uid();
        break;
    case 2:
        printf("seteuid(2000)\n");
        seteuid(2000);
        print_uid();
        break;
    case 3:
        printf("setresuid(-1, 2000, 3000)\n");
        setresuid(-1, 2000, 3000);
        print_uid();
        break;
    default:
        printf("Wrong argument (not in accepted 0-3 range)");
        return 1;
        break;
    }
}
