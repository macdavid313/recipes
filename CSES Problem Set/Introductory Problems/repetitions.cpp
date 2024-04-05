#include <iostream>

using namespace std;

int main() {
  string S;
  getline(cin, S);

  int best = 1, accum = 1;
  for (string::size_type i = 1; i < S.size(); i++) {
    if (S[i] == S[i - 1]) {
      accum += 1;
    } else {
      accum = 1;
    }
    best = max(best, accum);
  }

  cout << best << endl;
}
