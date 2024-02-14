bool my_access(struct stat *sb, int mode){
    uid = getuid()
    groups = getgroups();
    R = R_OK & mode;
    W = W_OK & mode;
    X = X_OK & mode;
    fmode = sb.st_mode;

    if(uid == 0) return true;
    if(uid == sb.st_uid) {
        return (!R or (S_IRUSR & fmode)) and
               (!W or (S_IWUSR & fmode)) and
               (!X or (S_IXUSR & fmode));
    }
    else if sb.st_gid is in groups:
        return (!R or (S_IRGRP & fmode)) and
               (!W or (S_IWGRP & fmode)) and
               (!X or (S_IXGRP & fmode))

    else
        return (!R or (S_IROTH & fmode)) and
               (!W or (S_IROTH & fmode)) and
               (!X or (S_IROTH & fmode)) 
}
