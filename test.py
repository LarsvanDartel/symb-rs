from symb_rs import run

while True:
    try: 
        records = run(input("calc > "))
    except KeyboardInterrupt:
        print()
        break
    except ValueError as e:
        print(e)
        continue
    else:
        print(records, end="")