#include "csapp.h"

/* 
 char    *pw_name   User's login name. 
uid_t    pw_uid    Numerical user ID. 
gid_t    pw_gid    Numerical group ID. 
char    *pw_dir    Initial working directory. 
char    *pw_shell  Program to use as shell. 


*/
static const char *uidname(uid_t uid) {
  /* TODO: Something is missing here! */
  struct passwd *pw = getpwuid(uid);
  return pw->pw_name;
}

static const char *gidname(gid_t gid) {
  /* TODO: Something is missing here! */
  struct group *gr = getgrgid(gid);
  return gr->gr_name;
}

static int getid(uid_t *uid_p, gid_t *gid_p, gid_t **gids_p) {
  gid_t *gids = NULL;
  int ngid = 2;
  int groups;

  /* TODO: Something is missing here! */
  *uid_p  = getuid();
  *gid_p  = getgid();

  gids = (gid_t *)malloc(sizeof(gid_t)*ngid);
  while((groups = getgroups(ngid,gids)) < 0){
    ngid *= 2;
    gids = (gid_t *)realloc(gids, sizeof(gid_t)*ngid);
  }

  *gids_p = gids;
  return groups;
}

int main(void) {
  uid_t uid;
  gid_t *gids, gid;
  int groups = getid(&uid, &gid, &gids);

  printf("uid=%d(%s) gid=%d(%s) ", uid, uidname(uid), gid, gidname(gid));
  printf("groups=%d(%s)", gids[0], gidname(gids[0]));
  for (int i = 1; i < groups; i++)
    printf(",%d(%s)", gids[i], gidname(gids[i]));
  putchar('\n');

  free(gids);

  return 0;
}
