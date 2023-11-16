#include "csapp.h"

#define DIRBUFSZ 256

static void print_mode(mode_t m) {
  char t;

  if (S_ISDIR(m))
    t = 'd';
  else if (S_ISCHR(m))
    t = 'c';
  else if (S_ISBLK(m))
    t = 'b';
  else if (S_ISREG(m))
    t = '-';
  else if (S_ISFIFO(m))
    t = 'f';
  else if (S_ISLNK(m))
    t = 'l';
  else if (S_ISSOCK(m))
    t = 's';
  else
    t = '?';

  char ur = (m & S_IRUSR) ? 'r' : '-';
  char uw = (m & S_IWUSR) ? 'w' : '-';
  char ux = (m & S_IXUSR) ? 'x' : '-';
  char gr = (m & S_IRGRP) ? 'r' : '-';
  char gw = (m & S_IWGRP) ? 'w' : '-';
  char gx = (m & S_IXGRP) ? 'x' : '-';
  char or = (m & S_IROTH) ? 'r' : '-';
  char ow = (m & S_IWOTH) ? 'w' : '-';
  char ox = (m & S_IXOTH) ? 'x' : '-';

  /* TODO: Fix code to report set-uid/set-gid/sticky bit as 'ls' does. */
  ux = (m & S_ISUID) ? 's' : ux;
  gx = (m & S_ISGID) ? 's' : gx;
  ox = (m & S_ISVTX) ? 't' : ox;

  printf("%c%c%c%c%c%c%c%c%c%c", t, ur, uw, ux, gr, gw, gx, or, ow, ox);
}

static void print_uid(uid_t uid) {
  struct passwd *pw = getpwuid(uid);
  if (pw)
    printf(" %10s", pw->pw_name);
  else
    printf(" %10d", uid);
}

static void print_gid(gid_t gid) {
  struct group *gr = getgrgid(gid);
  if (gr)
    printf(" %10s", gr->gr_name);
  else
    printf(" %10d", gid);
}

static void file_info(int dirfd, const char *name) {
  struct stat sb[1];

  /* TODO: Read file metadata. */
  Fstatat(dirfd, name, sb, 0);

  print_mode(sb->st_mode);
  printf("%4ld", sb->st_nlink);
  print_uid(sb->st_uid);
  print_gid(sb->st_gid);

  /* TODO: For devices: print major/minor pair; for other files: size. */
  if (S_ISCHR(sb->st_mode)) 
    printf(" %d, %d", major(sb->st_rdev), minor(sb->st_rdev));
  else 
    printf(" %5ld", sb->st_size);

  char *now = ctime(&sb->st_mtime);
  now[strlen(now) - 1] = '\0';
  printf("%26s", now);

  printf("  %s", name);

  if (S_ISLNK(sb->st_mode)) {
  /* TODO: Read where symlink points to and print '-> destination' string. */
    char linkBuf[BUFSIZ];
    Readlinkat(dirfd, name, linkBuf, BUFSIZ);
    printf(" -> %s", linkBuf);
  }

  putchar('\n');
}

int main(int argc, char *argv[]) {
  if (!argv[1])
    argv[1] = ".";

  int dirfd = Open(argv[1], O_RDONLY | O_DIRECTORY, 0);
  char buf[DIRBUFSZ];
  int n;

  while ((n = Getdents(dirfd, (void *)buf, DIRBUFSZ))) {
    struct linux_dirent *d;
    /* TODO: Iterate over directory entries and call file_info on them. */
    for(void* ptr = buf; ptr != buf + n; ptr += d->d_reclen){
      d = (struct linux_dirent *)ptr;
      if(d->d_name[0] != '.')
        file_info(dirfd, d->d_name);
    }
  }

  Close(dirfd);
  return EXIT_SUCCESS;
}
