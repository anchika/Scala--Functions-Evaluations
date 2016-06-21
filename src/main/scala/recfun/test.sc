def pascal(c : Int, r : Int): Int = {
  if ((c == 0) || (r == c)) 1
  else pascal(c-1, r -1) + pascal(c, r-1)
}
pascal(0,2)
def balance(chars : List[Char]) : Boolean = {
  def f(chars:List[Char], num : Int):Boolean ={
    if (chars.isEmpty) num==0
    else {
      val h = chars.head
      val n =
        if( h == '(') num + 1
        else if(h == ')') num - 1
        else num
      if(n >=0) f(chars.tail, n)
      else false
    }
  }
  f(chars, 0)
}

balance("Just(an exanpme".toList)

def countChange(money :Int, coins :List[Int]):Int ={
  if (money == 0) 1
  else if( money < 0) 0
  else if (coins.isEmpty) 0
  else countChange(money, coins.tail) + countChange(money - coins.head, coins)
}
val nums: List[Int] = List(1, 2)
countChange(4, nums)

def sumInts(a: Int, b:Int) : Int = {
  if (a>b) 0 else a + sumInts(a+1, b)
}


