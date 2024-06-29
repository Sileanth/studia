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
    print_uid();
}
