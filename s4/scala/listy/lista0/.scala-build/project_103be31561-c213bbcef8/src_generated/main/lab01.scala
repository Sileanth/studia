



object lab01 {
/*<script>*/import scala.util.control.Breaks._



//imperative implemenation
def scalarUgly(xs : List[Int], ys: List[Int]) =
  var scalar_product = 0
  var index = 0
  while (index < ys.length) {
    scalar_product = scalar_product + (xs(index) * ys(index))
    index = index + 1
  }
  scalar_product


//structural recursion with pattern matching
def scalarRecursive(xs: List[Int], ys: List[Int]) : Int = (xs, ys) match
	case (x :: xs, y :: ys) => x * y + scalarRecursive(xs, ys) 
	case (Nil, Nil) 	      => 0
	case (Nil, _) 	      => 0 //potential error, lists have diffrent length 
	case (_, Nil)	      => 0 //potential error, lists have diffrent length 

// for comprehension
def scalar(xs: List[Int], ys: List[Int]): Int = 
	var multiplied = for { (x, y) <- xs.zip(ys)} yield x * y
	multiplied.sum


def isPrimeUgly(n : Int): Boolean = 
	var ans = true
	var i = 2
	breakable { while (i < n) {
		if n % i == 0 
		then i = i + 1
		else ans = false; break
	}}
	ans

def isPrime(n : Int): Boolean =
	var numbers = 2 to (n-1)
	numbers.forall(x => {x % 2 == 0})


def primePairs(n: Int): List[(Int, Int)] = 
	val pairs = for {x <- 1 to n 
		y <- 1 to n} yield (x,y) 
	val prime_pairs = pairs.filter((x,y) => isPrime(x+y))
	prime_pairs.toList


val filesHere = new java.io.File(".").getPath
println(filesHere)
			
/*</script>*/ /*<generated>*/
def args = lab01_sc.args$
  /*</generated>*/
}

object lab01_sc {
  private var args$opt0 = Option.empty[Array[String]]
  def args$set(args: Array[String]): Unit = {
    args$opt0 = Some(args)
  }
  def args$opt: Option[Array[String]] = args$opt0
  def args$: Array[String] = args$opt.getOrElse {
    sys.error("No arguments passed to this script")
  }
  def main(args: Array[String]): Unit = {
    args$set(args)
    lab01.hashCode() // hasCode to clear scalac warning about pure expression in statement position
  }
}

