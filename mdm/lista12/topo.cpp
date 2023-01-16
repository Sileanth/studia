vector<int> topo(vector<vector<int>> g, vector<int> in) {
    vector<int> s, ans;
    for(int i = 0; i < in.size(); i++) {
        if(in[i] == 0) s.push_back(i);
    }
    while(s.size()) {
        int x = s.back(); s.pop_back();
        for(int v : g[x]) {
            in[v]--;
            if(in[v] == 0) {
                s.push_back(i);
                ans.push_back(i);
            }
        }
    }
    return ans;
}