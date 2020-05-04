#include "bdd-for-c.h"
#include "windows.h"

spec("mass") {
  it("should work") {
    check(strcmp("foo", "foo") == 0);
  }
}
