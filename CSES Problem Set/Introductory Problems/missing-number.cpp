#include <iostream>

using namespace std;

int main() {
  int N;
  cin >> N;

  int xnum = 0;
  for (int i = 1; i <= N; i++) {
    xnum ^= i;
  }

  int x;
  for (int i = 0; i < N - 1; i++) {
    cin >> x;
    xnum ^= x;
  }

  cout << xnum << endl;
}
