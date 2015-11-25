#include <iostream>
#include <stdlib.h>
#include <algorithm>
#include <vector>
#include <time.h>
using namespace std;

#define UNDEF -1
#define TRUE 1
#define FALSE 0

uint numVars;
uint numClauses;
vector<vector<int> > clauses;
vector<int> model;
vector<int> modelStack;
uint decisionLevel;

vector<vector<int> > clausesPos; // clauses[0][1] give the second clause where the lit 1 is positive
vector<vector<int> > clausesNeg;
vector<int> latencia;

vector<int> cabronasPos;
vector<int> cabronasNeg;

bool shiet( pair<int,int> a ,pair<int,int>b) {return (a.second>b.second);}


void readClauses( ){
    // Skip comments
    char c = cin.get();
    while (c == 'c') {
        while (c != '\n') c = cin.get();
        c = cin.get();
    }
    // Read "cnf numVars numClauses"
    string aux;
    cin >> aux >> numVars >> numClauses;
    clauses.resize(numClauses);
    clausesPos.resize(numVars+1);
    clausesNeg.resize(numVars+1);
    cabronasPos.resize(numVars+1,0);
    cabronasNeg.resize(numVars+1,0);
    latencia.resize(numVars);
    // Read clauses
    for (uint i = 0; i < numClauses; ++i) {
        int lit;
        while (cin >> lit and lit != 0) {
            clauses[i].push_back(lit);
            if (lit > 0) clausesPos[lit].push_back(i);
            else clausesNeg[-lit].push_back(i);
        }
    }
    vector<pair<int,int> >vpAux(numVars+1);
    for (int i = 1; i < numVars+1; ++i) {
        pair <int,int> pAux;
        pAux.first = i;
        pAux.second = min(clausesPos[i].size(),clausesNeg[i].size());
        vpAux[i] = pAux;

    }
    sort(vpAux.begin()+1, vpAux.end(), shiet);
    for (int i = 1; i < numVars+1; ++i) latencia[i-1] = vpAux[i].first;
}



int currentValueInModel(int lit){
    if (lit >= 0) return model[lit];
    else {
        if (model[-lit] == UNDEF) return UNDEF;
        else return 1 - model[-lit];
    }
}


void setLiteralToTrue(int lit){
    modelStack.push_back(lit);
    if (lit > 0) model[lit] = TRUE;
    else model[-lit] = FALSE;
}


bool propagateGivesConflict (vector<int> litsToPropagate) {
    bool fallo = false;
    int pSize = litsToPropagate.size();
    for(uint j = 0; j < pSize and not fallo; ++j) {
        vector<int> vAux;
        if (litsToPropagate[j] > 0) vAux = clausesNeg[litsToPropagate[j]];
        else vAux = clausesPos[abs(litsToPropagate[j])];
        int vSize = vAux.size();
        for (uint i = 0; i < vSize; ++i) {
            bool someLitTrue = false;
            int numUndefs = 0;
            int lastLitUndef = 0;
            for (uint k = 0; not someLitTrue and k < clauses[vAux[i]].size(); ++k){
                int val = currentValueInModel(clauses[vAux[i]][k]);
                if (val == TRUE) someLitTrue = true;
                else if (val == UNDEF){ ++numUndefs; lastLitUndef = clauses[vAux[i]][k]; }
            }
            if (not someLitTrue and numUndefs == 0) {
                fallo = true; // conflict! all lits false
//                cout << "SOMElitTRUE" << endl;
                if (litsToPropagate[j] > 0) ++cabronasPos[litsToPropagate[j]];
                else ++cabronasNeg[-litsToPropagate[j]];
//                cout << "notSOMElitTRUE" << endl;
            }
            else if (not someLitTrue and numUndefs == 1 and not fallo) {
                setLiteralToTrue(lastLitUndef);
                litsToPropagate.push_back(lastLitUndef);
                ++pSize;
            }
        }
    }
    return fallo;
}


void backtrack(){
    uint i = modelStack.size() -1;
    int lit = 0;
    while (modelStack[i] != 0){ // 0 is the DL mark
        lit = modelStack[i];
        model[abs(lit)] = UNDEF;
        modelStack.pop_back();
        --i;
    }
    // at this point, lit is the last decision
    modelStack.pop_back(); // remove the DL mark
    --decisionLevel;
    setLiteralToTrue(-lit);  // reverse last decision
}


// Heuristic for finding the next decision literal:
int getNextDecisionLiteral(){
//    cout << "getnext" << endl;
    int ret=-1;
    int cPSize = cabronasPos.size();
    int maximo = 0;
    for (uint i = 1; i < cPSize; ++i) {
        if (model[i] == UNDEF) {
        if (cabronasPos[i] > maximo or cabronasNeg[i] > maximo or ret==-1) { maximo = max(cabronasPos[i],cabronasNeg[i]); ret = i;}
        }
    }

//    cout << ret << endl;

    if (ret != -1) return ret;
    else return 0;

//    int pSize = posibles.size();
//    cout << "Hay este numero: " << pSize << " y el maximo es " << maximo << "el primero es: " << posibles[0];
//    for (uint i = 0; i < pSize; ++i)
//        if (model[abs(posibles[i])] == UNDEF) {cout << " olakase " << endl;return posibles[i];}
//        else cout << "Este ya está definido: " << posibles[i] << endl;
//            //            return clausesNeg[i].size() > clausesPos[i].size() ? latencia[i] : -latencia[i];  // returns first UNDEF var, positively

//            return 0; // reurns 0 when all literals are defined
}

void checkmodel(){
    for (int i = 0; i < numClauses; ++i){
        bool someTrue = false;
        for (int j = 0; not someTrue and j < clauses[i].size(); ++j)
            someTrue = (currentValueInModel(clauses[i][j]) == TRUE);
        if (not someTrue) {
            cout << "Error in model, clause is not satisfied:";
            for (int j = 0; j < clauses[i].size(); ++j) cout << clauses[i][j] << " ";
            cout << endl;
            exit(1);
        }
    }
}

int main(){
    clock_t t = clock();
    readClauses(); // reads numVars, numClauses and clauses
    model.resize(numVars+1,UNDEF);
    decisionLevel = 0;

    int decisionLit = getNextDecisionLiteral();
    modelStack.push_back(0);  // push mark indicating new DL
    ++decisionLevel;
    setLiteralToTrue(decisionLit);
    vector<int> litsToPropagate(1);
    litsToPropagate[0] = decisionLit;

    // DPLL algorithm
    while (true) {
        while ( propagateGivesConflict(litsToPropagate) ) {
            if ( decisionLevel == 0) { cout << (clock()-(float)t)/CLOCKS_PER_SEC << " UNSATISFIABLE" << endl; return 10; }
            backtrack();
            litsToPropagate[0] = modelStack[modelStack.size()-1];
        }
        decisionLit = getNextDecisionLiteral();
        if (decisionLit == 0) { checkmodel(); cout << (clock()-(float)t)/CLOCKS_PER_SEC << " SATISFIABLE" << endl; return 20; }
        // start new decision level:
        modelStack.push_back(0);  // push mark indicating new DL
        ++decisionLevel;
        setLiteralToTrue(decisionLit);    // now push decisionLit on top of the mark
        litsToPropagate[0] = decisionLit;
    }
}