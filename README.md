## Description

This project is about counting primes and getting the nth prime.

The implementation is based on the fundamental idea of the inclusion-exclusion principle from the French mathematician Adrien-Marie Legendre.

The inclusion-exclusion principle used by Legendre to count primes is smarter and faster than the usual naive/brute-force because it avoids looking in vain for a prime divisor up to the square root of n everytime that n is prime.

However, improvements from Legendre's idea have made their way since, with notably :

- Lehmer's Method
- Meissel-Lehmer Algorithm
- Lagarias-Miller-Odlyzko Algorithm
- Deleglise-Rivat Algorithm

The new implementation used in this project has been designed with focus set on finding a good compromise between time and space, but also between efficiency and understanding of what the algorithm does under the hood.

This implementation could be seen like a seed that could grow slowly slowly once it has been planted.

It could also be used to create the core of a micro-service that would allow paralellism of internal computations on a cluster of several nodes.

Obviously, this implementation allows the computation of phi(n), which is the number of primes from 1 to n.

This implementation allows also the computation of the number of prime until n starting from any natural integer i between 1 and n.

Finally, this implementation allows the computation of the nth prime, for any given natural integer != 0, which is the inverse function of phi(n).


## Installation

1. Clone the repository:
    ```sh
    git clone https://github.com/arie-benichou/prime-counting-function.git
    ```
2. Navigate to the project directory:
    ```sh
    cd prime-counting-function
    ```
3. If you dont have a Scala environement yet, you can easily install one like this:
    ```sh
    ./coursier-install.sh
     ```
4. Once you have a Scala environement correctly installed, you should be able to launch an SBT shell:
    ```sh
    sbt
     ```
5. Inside the SBT shell, you should be able to compile the project by typing "compile":
    ```sh
    sbt:root> compile
     ```
5. You should also be able to run the tests by typing "test":
    ```sh
    sbt:root> test
     ```
6. Finally, you should be able to run the program:
    ```sh
    sbt:root> runMain PrimesCounter
     ```          

## Usage

1. Count primes between 1 and 100:
    ```sh
    runMain PrimesCounter 1 100
     ```

2. Count primes between 100 and 127:
    ```sh
    runMain PrimesCounter 100 127
     ```

3. Get the prime of rank 31:
    ```sh
    runMain PrimesCounter 31
     ```

## License

This project is licensed under GNU Public Licence version 3 [GPL v3](http://www.gnu.org/copyleft/gpl.html)