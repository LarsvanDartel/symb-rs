from symb_rs import run

while True:
    try: 
        run(input("calc > "))
    except KeyboardInterrupt:
        print()
        break