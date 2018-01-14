import scala.util.control.Breaks._
import scala.collection.mutable.ListBuffer

object pp3 {
 
 class KMPSearch(){
    var count=0
    def makeTemporaryArray(pattern:Array[Char]):Array[Int]={
      var lps = new Array[Int](pattern.length);
		  var index=0;
		  var i = 1;
		  while(i < pattern.length){
		    //count=KMPSearch.inc
			  if(pattern(i) == pattern(index)){
  				lps(i) = index + 1;
  				index += 1;
  				i += 1;
			  }
			  
			  else{
			  	if(index!=0)
			  	{
					  index=lps(index - 1);
				  }
			  	else{
					  lps(i)= 0;
					  i=i+1
				  }
			  }
		  }
		  return lps;
      }
    
    def SearchByKMP(text:Array[Char],pattern:Array[Char]):Boolean={
      var lps = makeTemporaryArray(pattern);
		  var i=0;
		  var j=0;
		  while(i<text.length && j<pattern.length){
		    count=KMPSearch.inc
  			if(text(i)==pattern(j)){
  				i += 1;
  				j += 1;
  			}
  			else{
  				if(j != 0){
  					j = lps(j-1);
  				}
  				else{
  					i += 1;
  				}
  			}
  		}
		  println("Number of Key Comparisons for KMP are: "+count)
  		if(j==pattern.length)
  		{
  			return true;
  		}
  		return false;
    }
  }
      object KMPSearch{
      private var current=0
      private def inc={current += 1;current}
    }
 
 def bmptable(text: List[Char] , pat:List[Char])
  {
   var pattern=pat
   var p = pattern.distinct
   p ++= ListBuffer('*')
   var x=0
   var y=10000
   var bmp = ListBuffer.fill(p.length)(0)
   var z=0
   var num=0
   for(  x <- 0 to pattern.length-1  ){
       
       breakable
       {
         for(z <- 0 to p.length-1)
         {
           if(pattern(x)==p(z))
           { 
             
             y=z
             break;
           }
         }
       } 

       bmp(y) = pattern.length-x-1
       
    }
   for (z<-0 to p.length-2){
   if (pattern(pattern.length-1)==p(z))
   {
     
     bmp(z)=pattern.length
   }
   }
   
   bmp(p.length-1)=pattern.length
   z=pattern.length-1
   while (z<=text.length-1)
   {
	   
     var count:Int=0
     
     if (text(z)==pattern(pattern.length-1))
     {
        var a=z
        breakable  
       {
         for (x<-pattern.length-1 to 0 by -1)
         {
          num=num+1
          if(text(a)==pattern(x)) 
          {
            
            count=count+1
           a=a-1
            if(count==pattern.length)
             {
               println("Match Found for Boyer Moore")
               z=100
             }
             
          }
          else 
          {
                  
                 z=z+bmp(p.indexOf(text(z)))
               
                 break
          }
          
         }
       }
     }
       
       
     
	   else
       { 
	       num=num+1
         if(p.indexOf(text(z))>=0)
         { 
           
           z=z+bmp(p.indexOf(text(z))) 
         }
         else
         {        
           
           z=z+pattern.length-1
         }
         
       //z=z+1
       }
   
 
    }
   
   
   println("The number of key comparisons for Boyer Moore are: "+num)
   
  }
  

  
  
 def main(args:Array[String])
  {
    try
    {
      var t = readLine("Enter the text: ")
      var text= t.toList
      var p= readLine("Enter the pattern you want to search: ");
      var pattern = p.toList 
      println("First We will do Bayer Moore's string Matching")
      bmptable(text,pattern)
      println("\nNow Performing KMP on the string ")
      val k=new KMPSearch()
      if ((k.SearchByKMP(t.toArray,p.toArray))==true)
      {
        println("The main string contains the substring KMP")  
      }
      else
      {
        println("The main string does not contain the substring")
      }

    }
    catch
      {
        case _ : Throwable => println("Exception!!!! Check your inputs ...")
      }
  }
  
  
}

