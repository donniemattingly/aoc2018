#!/usr/bin/swift

func sum_of_multiples_under(n: Int, multiples: Int...) -> Int {
    return (0 ..< n)
        .filter { i in
            multiples.reduce(false, { acc, val in
                acc || i % val == 0
            })
        }.reduce(0, { x, y in
            x + y
        })
}

func problem_one() {
    print(sum_of_multiples_under(n: 1000, multiples: 3, 5))
}

problem_one()

