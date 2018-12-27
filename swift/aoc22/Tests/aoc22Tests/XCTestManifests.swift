import XCTest

#if !os(macOS)
public func allTests() -> [XCTestCaseEntry] {
    return [
        testCase(aoc22Tests.allTests),
    ]
}
#endif