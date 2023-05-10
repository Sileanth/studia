#include<bits/stdc++.h>
#include <cstddef>
using namespace std;
 
// An AVL tree node
struct Node
{
    long long v;
    Node * l;
    Node * r;
    int h;

};
 
int wys(Node * n)
{
    if (n == NULL)
        return 0;
    return n->h;
}

int max_wys(Node * x, Node * y) {
  return max(wys(x), wys(y));
}

void update_h(Node * n) {
  n->h = max_wys(n->l, n->r) + 1;
};
//assume that is propper construction
Node* node(long long x, Node * l, Node * r) {
  Node * n = new Node();
  n->v = x;
  n->l = l;
  n->r = r; 
  n->h = max_wys(l, r);
  return n;
}

 
Node* singlet(long long x)
{
   return node(x, NULL, NULL); 
}
 

Node* prawak(Node *n)
{
    Node *nl = n->l; 
    Node *nlr = nl->r;
 
    nl->r = n;
    n->r = nlr;
 
   update_h(n);
   update_h(nl);
 
   return nl;
}
 

Node* lewak(Node * n)
{
    Node *nr = n->r;
    Node *nrl = nr->l;
 
    nr->l = n;
    n->r = nrl;
 
   update_h(n);
   update_h(nr);
   
   return nr;
}
 

Node* rebalance(Node* n, long long x) {
  update_h(n);
  int dif = n->l - n->r;
  if(dif < -1 && x > n->r->v) return lewak(n);
  else if(dif > 1 && x < n->l->v) return prawak(n);
  else if(dif < -1 && x < n->r->v) {
    n->r =  prawak(n->r);
    return lewak(n);
  }
  else if(dif > 1 && x > n->l->v) {
    n->l = lewak(n->l);
    return prawak(n);
  }
  
  return n;

}


Node* ins(Node* n, long long x) {
  //Najpierw znajdujemy tak ja w zwykłym bst i wstawiamy
  if(n == NULL) return singlet(x);
  else if(n->v == x) return n; //bez duplikatów
  else if(x < n->v) return ins(n->l,x);
  else return ins(n->r,x);

  //tylko jak wspinamy się z powrotem to naprawiamy
  return rebalance(n, x);
  
}


Node* find(Node* n, long long x) {
  if(n == NULL || n->v == x) return n;
  else if(x < n->v) return find(n->l,x);
  else return find(n->r, x);
}

Node* upper(Node* n, long long x) {
  
}

int main() {
  Node* t = NULL;



};

