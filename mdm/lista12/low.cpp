int dfs(int x, int d, vector<vector<int>>& g, 
    int& ans, vector<bool>& odw) 
{
    odw[x] = true;
    int low = d;
    
    int cnt = 0; 
    for(int v : g[x]) {
        if(!odw[v]) {
            low = max(low, dfs(v, d+1, g, ans, odw));
            cnt++;
        }
    }
    if(d == 0 && cnt > 1) ans = x;
    if(low <= d && d != 0) ans = x;
    return low;
}   