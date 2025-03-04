#include <iostream>
#include <algorithm>
#include <vector>
using namespace std;

vector<vector<int>> chunkIt(int n, const vector<int>& xs) {
    vector<vector<int>> result;
    for (size_t i = 0; i < xs.size(); i += n) {
        vector<int> chunk(xs.begin() + i, xs.begin() + min(i + n, xs.size()));
        result.push_back(chunk);
    }
    return result;
}



#include <numeric>

int firstLoc(int target, const vector<int>& xs) {
    auto it = find(xs.begin(), xs.end(), target);
    if (it != xs.end()) {
        return distance(xs.begin(), it);  // Return 0-based index
    }
    return -1;  // Return -1 if not found
}

int main() {
    auto result = chunkIt(2, {1, 2, 3, 4, 5});
    for (auto &vec : result) {
        for (int x : vec) std::cout << x << " ";
        std::cout << "\n";
    }

    std::cout << "First loc: " << firstLoc(3, {1, 2, 3, 4, 3}) << "\n";
    return 0;
}

