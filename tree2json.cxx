#include "json_spirit.h"
#include <iostream>

using namespace json_spirit;

int main(int argc, char *argv[]) {

    Object root;
    Value h("hello");
    root.push_back(Pair("h", "hello"));

    std::cout << write(root);

    return 0;
}
