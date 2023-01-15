bool dfs(int x, int col, vector<vector<int>>& g, vector<int>& color) {
    color[x] = col;
    for(int v : g[x]) {
        if(color[v] == c) return false;
        if(color[v] == 0) dfs(v, -col, g, color);
    }
    return true;
}