#!/usr/bin/env python3


def process_number(number):
    """
    For multiples of three returns "Three" instead of the number.
    For the multiples of five returns "Five".
    For numbers which are multiples of both three and five returns "ThreeFive".
    """
    if number == 0:
        return "0"
    if number % 3 == 0 and number % 5 == 0:
        return "ThreeFive"
    elif number % 3 == 0:
        return "Three"
    elif number % 5 == 0:
        return "Five"
    else:
        return str(number)


def print_numbers(cnt: int = 100) -> None:
    """
    Prints numbers from 0 to ``cnt``.
    """
    for i in range(cnt):
        print(process_number(i))


if __name__ == "__main__":
    print_numbers(100)
