package main

import ("fmt"
         "strconv"
         "errors"
         "bufio"
         "os"
         "strings"
         "reflect"
         
)
       



//go 1.6.2



    


//q1


func numberreverse (n int) int {

    n3:=n; reversedNumber:= 0; remainder:=0

    for n3!=0  {

 remainder = n3%10
 reversedNumber = reversedNumber*10 + remainder
 n3 /= 10
 }
return reversedNumber   
}


func prime(n int) bool{
    n1:=n
     r1:=0
   for i:=2;i<=n1;i++{
      if(n1%i==0){
         r1++

            }
            }

    if(r1>1){

    return false

    }else{
       return true}
       }

  func countEmirpsLessThan(n int) int {

    n5:=n
    count:=0
     for i:=0;i<=n5;i++{
       result3:=prime(i)
         if(result3==true){
            num1:=numberreverse(i)
             result4:=prime(num1)

            if(result4==true && result3==true && num1!= i){

                count++}}
   }

    return count

} 


//q2

func countWords(filename string) (map[string]int,error) {
 //https://www.youtube.com/watch?v=2LSkVEbFYyw&list=WL&index=77&t=1170s
 //youtube video
    s, err := os.Open(filename)
     if(err != nil){
      panic(err)
     }
    scanner := bufio.NewScanner(s)

    //scanning every word 
    scanner.Split(bufio.ScanWords)

    //Create an empty map to store each different String
    MapStrings := make(map[string]int)


    for scanner.Scan() {
        word := scanner.Text()
      
       //scanning through all the word if it matches then increase the count
        if _, x := MapStrings[word]; x {
        	MapStrings[word]++
        }else{
        	MapStrings[word] = 1
        }
    }
    return MapStrings,err
}


//q3
type Time24 struct {
    hour, minute, second uint8
}

func equalsTime24(a Time24, b Time24)bool{
    if(a.hour==b.hour && a.minute==b.minute && a.second==b.second){
        return true;}else{ return false;}
}
//a comes strictly before b
func lessThanTime24(a Time24, b Time24)bool{
    if(a.hour<b.hour){
        return true;
    }else if(a.hour==b.hour&&a.minute<b.minute){
        return true;
    }else if(a.hour==b.hour&&a.minute==b.minute  &&a.second<b.second){
        return true;
    }  else{
        return false;}
  
    
   }

func (t Time24) String() string{
	
	timestring:= ""

	//Check if Hour is greater than 9 else it needs a leading 0

	if (t.hour > 10){
		timestring = timestring + strconv.Itoa(int(t.hour)) + ":"
       
	}else{
         timestring = timestring + "0"+strconv.Itoa(int(t.hour)) + ":";
		
	}

	//Then check if minute is greater than 9 else it needs a leading 0

	if (t.minute > 10){
		timestring = timestring + strconv.Itoa(int(t.minute)) + ":"
        
	}else{
		timestring = timestring + "0"+strconv.Itoa(int(t.minute)) + ":"
	}
	//Then check if second is greater than 9 else it needs a leading 0

	if (t.second > 10){
		timestring = timestring + strconv.Itoa(int(t.second))
        
	}else{
		timestring = timestring + "0"+strconv.Itoa(int(t.second))
	}		
	return timestring
}

func (t Time24) validTime24() bool{
    if(t.hour>=0 && t.hour<=23 && t.minute>=0 && t.minute <60 && t.second>=0 && t.second<60){
        return true;
    }else{
        return false;
    }
}
    
func minTime24(times []Time24) (Time24, error){
	if len(times) == 0{
		
		// Empty time returned with a error message
		err:= errors.New("Empty arg.")
		return Time24{0,0,0}, err
	}else{
		//Sorting to get the minimum 
    //at first minimum is the first element
		minimum := times[0]
		//As it encounters any valid time less than minimum then it assigns the value to be new minimum
		for i := 1; i< len(times); i++{
			if ( lessThanTime24(times[i],minimum) == true ){
				minimum = times[i]
			}
		}
		return minimum, nil
	}
}

//q4
func linearSearch(x interface{}, lst interface{}) int {

      Listofinterface := reflect.ValueOf(lst) //getting value of interface
       if (Listofinterface.Len() == 0 ){
		return -1
	}
 // https://golang.org/pkg/reflect/#example_Kind 
   
    // Get the type of the inputs x and lst using reflect.TypeOf()
	if ( reflect.TypeOf(x) != reflect.TypeOf(lst).Elem() ){
		//From https://gobyexample.com/panic
		panic("Type do not match")
	}

	
	// Index(i).Interface() returns value at ith index
	for i:= 0 ; i < Listofinterface.Len(); i++ {
		if (Listofinterface.Index(i).Interface() == x){
			return i
		}
	}

 return -1	
}

//q5


func allBitSeqs(n int) [][]int {
//if n greater than or equal to one then generate the bit sequence and get slice length of 2^n by multiplying 2 n times
	
	if n>=1{
		sliceLen:=1
           for i:=0;i<n;i++{
              sliceLen=sliceLen*2             
                }
	
	sliceSequence := make([][]int,sliceLen)

	//Bit sequence are the numbers from 0 to Two  to the power n -1 number
	//This loop processes each bit sequence at a time
	//loop runs for Two to the power n times to get all sequence of bits
	     for i:= 0; i< sliceLen; i++{
		int_Array := make([]int,n)
		x := int64(i)

               //https://yourbasic.org/golang/convert-int-to-string/#string-to-int-int64
		binarynum := strconv.FormatInt(x,2)
              //leading 0s to make it length of n
		
		len_bin:=len(binarynum)
		len_bin2:=n-len_bin
		for i:=0;i<len_bin2;i++{
		       binarynum = "0" + binarynum
		}
		//chararray to hold every element of a string
		C_Array := strings.Split(binarynum,"")

		for i:=0; i<n; i++{
			
			char_to_int, err := strconv.Atoi(C_Array[i])
				if err != nil{
					panic(err)
				}
			//Put the converted char to the resulting Integer Array	
			int_Array[i] = char_to_int	
		} 

		//slicesequence holding each bit sequence
		sliceSequence[i] = int_Array
	}
	return sliceSequence
	}else{
	return make([][]int,0)}
	
} 
























                                              