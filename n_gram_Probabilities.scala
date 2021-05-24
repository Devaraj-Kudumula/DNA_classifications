package assignments

import scala.io.Source

object n_gram_Probabilities extends App{
  def k_mers(k: Int, sequence: String): List[String] = {
    var kmer=List[String]()
    var ini=0
    while(ini<sequence.length){
      if(ini+k<sequence.length) {
        kmer :+= sequence.substring(ini, ini + k)
        ini += k
      }
      else{
        kmer :+=sequence.substring(ini,sequence.length)
        ini+=k
      }
    }
    kmer
  }
  var lines:List[String] = Source.fromFile("F:/amrita/SEM-IV/INTELLIGENCE OF BIOLOGICAL SYSTEM - IV/MN908947.txt").getLines.toList
  lines = List(lines.mkString(""))

  var ten_mers_list = k_mers(10,lines(0))
  var ten_mers_fin = ten_mers_list.reduce(_+" "+_)
  println(s"k-mers when k = 10 $ten_mers_fin")

  var twenty_mers_list = k_mers(20,lines(0))
  var twenty_mers_fin = twenty_mers_list.reduce(_+" "+_)
  println(s"k-mers when k = 20 $twenty_mers_fin")

  var thirty_mers_list = k_mers(30,lines(0))
  var thirty_mers_fin = thirty_mers_list.reduce(_+" "+_)
  println(s"k-mers when k = 30 $thirty_mers_fin")

  var fourty_mers_list = k_mers(40,lines(0))
  var fourty_mers_fin = fourty_mers_list.reduce(_+" "+_)
  println(s"k-mers when k = 40 $fourty_mers_fin")

  def ngrams(s:List[String],n:Int)={
    var ngramss:List[List[String]] = List()
    var i1 = 1
    while(s.slice(i1,i1+n).length==n){
      ngramss = s.slice(i1,i1+n) :: ngramss
      i1 = i1+1
    }
    ngramss.reverse
  }
  var bigrams:List[List[String]]=ngrams(ten_mers_list,2)
  val bigrams_grp = bigrams.groupBy(identity).mapValues(_.size)
  val bigrams_sum:Float = bigrams_grp.values.sum 
  val bigrams_probab = bigrams.groupBy(identity).mapValues(_.size/bigrams_sum)
  val fin_probab = bigrams_probab.toList
  println(s"Bi grams  with 10 mers")
  println(s"list of Bi-Grams is \n $bigrams")
  println(s"Probability of each value is \n $fin_probab")
  println("\n")

  var trigrams:List[List[String]]=ngrams(ten_mers_list,3)
  val trigrams_grp = trigrams.groupBy(identity).mapValues(_.size)
  val trigrams_sum:Float = trigrams_grp.values.sum
  val trigrams_probab = trigrams.groupBy(identity).mapValues(_.size/trigrams_sum)
  val final_probab = trigrams_probab.toList
  println(s"Tri grams  with 10 mers")
  println(s"list of Tri-Grams is \n  $trigrams")
  println(s"Probability of each value is \n $final_probab")
  println("\n")
  var bigrams_20:List[List[String]]=ngrams(twenty_mers_list,2)
  val bigrams_grp_20 = bigrams_20.groupBy(identity).mapValues(_.size)
  val bigrams_sum_20:Float = bigrams_grp_20.values.sum
  val bigrams_probab_20 = bigrams_20.groupBy(identity).mapValues(_.size/bigrams_sum_20)
  val fin_probab_20 = bigrams_probab_20.toList
  println(s"Bi grams with 20 mers")
  println(s"list of Bi-Grams is $bigrams_20")
  println(s"Probability of each value is $fin_probab_20")
  println("\n")
  var trigrams_20:List[List[String]]=ngrams(twenty_mers_list,3)
  val trigrams_grp_20 = trigrams_20.groupBy(identity).mapValues(_.size)
  val trigrams_sum_20:Float = trigrams_grp_20.values.sum
  val trigrams_probab_20 = trigrams_20.groupBy(identity).mapValues(_.size/trigrams_sum_20)
  val final_probab_20 = trigrams_probab_20.toList
  println(s"Tri grams with 20 mers")
  println(s"list of Tri-Grams is $trigrams_20")
  println(s"Probability of each value is $final_probab_20")
  println("\n")

  var bigrams_30:List[List[String]]=ngrams(thirty_mers_list,2)
  val bigrams_grp_30 = bigrams_30.groupBy(identity).mapValues(_.size)
  val bigrams_sum_30:Float = bigrams_grp_30.values.sum
  val bigrams_probab_30 = bigrams_30.groupBy(identity).mapValues(_.size/bigrams_sum_30)
  val fin_probab_30 = bigrams_probab_30.toList
  println(s"Bi grams with 30 mers")
  println(s"list of Bi-Grams is $bigrams_30")
  println(s"Probability of each value is $fin_probab_30")
  println("\n")
  var trigrams_30:List[List[String]]=ngrams(thirty_mers_list,3)
  val trigrams_grp_30 = trigrams_30.groupBy(identity).mapValues(_.size)
  val trigrams_sum_30:Float = trigrams_grp_30.values.sum
  val trigrams_probab_30 = trigrams_30.groupBy(identity).mapValues(_.size/trigrams_sum_30)
  val final_probab_30 = trigrams_probab_30.toList
  println(s"Tri grams with 30 mers")
  println(s"list of Tri-Grams is $trigrams_30")
  println(s"Probability of each value is $final_probab_30")
  println("\n")

  var bigrams_40:List[List[String]]=ngrams(fourty_mers_list,2)
  val bigrams_grp_40 = bigrams_40.groupBy(identity).mapValues(_.size)
  val bigrams_sum_40:Float = bigrams_grp_40.values.sum
  val bigrams_probab_40 = bigrams_40.groupBy(identity).mapValues(_.size/bigrams_sum_40)
  val fin_probab_40 = bigrams_probab_40.toList
  println(s"Bi grams with 40 mers")
  println(s"list of Bi-Grams is $bigrams_40")
  println(s"Probability of each value is $fin_probab_40")
  println("\n")
  var trigrams_40:List[List[String]]=ngrams(fourty_mers_list,3)
  val trigrams_grp_40 = trigrams_40.groupBy(identity).mapValues(_.size)
  val trigrams_sum_40:Float = trigrams_grp_40.values.sum
  val trigrams_probab_40 = trigrams_40.groupBy(identity).mapValues(_.size/trigrams_sum_40)
  val final_probab_40 = trigrams_probab_40.toList
  println(s"Tri grams with 40 mers")
  println(s"list of Tri-Grams is $trigrams_40")
  println(s"Probability of each value is $final_probab_40")
  //BY Devaraj Kudumula
}
