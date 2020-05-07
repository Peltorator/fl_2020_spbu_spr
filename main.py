import sys
import parser

def solve(conversions):
    if isinstance(conversions, list):
        for conversion in conversions:
            print(conversion[0] + " -> " + " ".join(conversion[1]))
    elif parser.my_error != "":
        print(parser.my_error)

if __name__ == "__main__":
    inp = sys.argv[1]
    with open(inp, "r") as inp_f:
        solve(parser.parser.parse(inp_f.read()))
