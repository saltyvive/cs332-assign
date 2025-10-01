package patmat

import common._

/**
 * Assignment 4: Huffman coding
 *
 */
object Huffman {

  /**
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   * leaves.
   */
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree



  // Part 1: Basics

  def weight(tree: CodeTree): Int = tree match{
    case f : Fork => f.weight
    case l : Leaf => l.weight
  }

  def chars(tree: CodeTree): List[Char] = tree match{
    case Fork(_, _, c_l, _) => c_l
    case Leaf(c, _) => List(c)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))



  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
  def times(chars: List[Char]): List[(Char, Int)] = {
    def gen_pair_list(chars: List[Char], paired_list: List[(Char, Int)]): List[(Char, Int)] = (chars, paired_list) match {
      case (Nil, res) => res
      case (h :: t, p_l) => {
        def add_pair(h: Char, p_l: List[(Char, Int)]): List[(Char, Int)] = p_l match{
          case Nil => (h, 1) :: Nil
          case (h_ch, cnt) :: t_pair_list if h_ch == h => (h_ch, cnt + 1) :: t_pair_list
          case h_pair :: t_pair_list => h_pair :: add_pair(h, t_pair_list)
        }

        gen_pair_list(t, add_pair(h, p_l))
      }
    }

    gen_pair_list(chars, List())
  }

  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    def sorting_list(freqs: List[(Char, Int)], sorted_leaf_list: List[Leaf]): List[Leaf] = (freqs, sorted_leaf_list) match{
      case (Nil, res) => res
      case (h_pair :: t_pair_list, acc_sorted_leaf_list) => {
        def add_leaf(h_pair: (Char, Int), cur_l_l: List[Leaf]): List[Leaf] = cur_l_l match{
          case Nil => Leaf(h_pair._1, h_pair._2) :: Nil
          case h_leaf :: t_leaf_list if h_leaf.weight >= h_pair._2 => Leaf(h_pair._1, h_pair._2) :: cur_l_l
          case h_leaf :: t_leaf_list => h_leaf :: add_leaf(h_pair, t_leaf_list)
        }

        sorting_list(t_pair_list, add_leaf(h_pair, acc_sorted_leaf_list))
      }
    }

    sorting_list(freqs, List())
  }

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = trees match{
    case h :: Nil => true
    case _ => false
  }

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] = trees match{
    case node1 :: node2 :: rest => {
      val new_fork = makeCodeTree(node1, node2)
      val removed_trees = rest

      def add_fork(new_fork: Fork, cur_trees: List[CodeTree]): List[CodeTree] = cur_trees match{
        case Nil => new_fork :: Nil
        case h_codetree :: t_codetree_list if(weight(h_codetree) >= weight(new_fork)) => new_fork :: cur_trees 
        case h_codetree :: t_codetree_list => h_codetree :: add_fork(new_fork, t_codetree_list)
      }

      add_fork(new_fork, removed_trees)
    }
    case _ => trees
  }

  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   *
   * Hint: before writing the implementation,
   *  - start by defining the parameter types such that the above example invocation
   *    is valid. The parameter types of `until` should match the argument types of
   *    the example invocation. Also define the return type of the `until` function.
   *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
   */
  def until(end_sign: List[CodeTree] => Boolean, combine_tree: List[CodeTree] => List[CodeTree]) (trees: List[CodeTree]): List[CodeTree] = if (end_sign(trees)) trees else until(end_sign, combine_tree)(combine_tree(trees))
  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree = until(singleton, combine)(makeOrderedLeafList(times(chars))).head



  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def decoding_bits_to_charlist(remaining_bits: List[Bit], tree: CodeTree, acc_char_list: List[Char]): List[Char] = remaining_bits match{
      case Nil => acc_char_list
      case _ => {
        def descending_until_leaf(cur_bits: List[Bit], cur_tree: CodeTree): (List[Bit], Char) = cur_tree match{
          case Leaf(c, _) => (cur_bits, c)
          case Fork(l, r, _, _) => if (cur_bits.head == 0) descending_until_leaf(cur_bits.tail, l) else descending_until_leaf(cur_bits.tail, r)
        }
        
        val descending_res = descending_until_leaf(remaining_bits, tree)
        decoding_bits_to_charlist(descending_res._1, tree, descending_res._2 :: acc_char_list)
      }
    }

    decoding_bits_to_charlist(bits, tree, List()).reverse
  }

  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = decode(frenchCode, secret)



  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def encoding_text_to_bit(remaining_text: List[Char], tree: CodeTree, acc_bit_list: List[Bit]): List[Bit] = remaining_text match{
      case Nil => acc_bit_list
      case _ => {
        def descending_until_leaf(cur_text: List[Char], cur_tree: CodeTree, direction: List[Int]): List[Bit] = cur_tree match{
          case Leaf(c_leaf, _) => if(c_leaf == cur_text.head) direction.reverse else List()
          case Fork(l, r, _, _) => {
            descending_until_leaf(cur_text, l,  0 :: direction) ::: descending_until_leaf(cur_text, r,  1 :: direction)
          }
        }

        encoding_text_to_bit(remaining_text.tail, tree, acc_bit_list ::: descending_until_leaf(remaining_text, tree, List()))
      }
    }

    encoding_text_to_bit(text, tree, List())
  }


  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = table match{
    case (c, b) :: t_codetable if c == char => b
    case h :: t => codeBits(t)(char)
  }

  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
  def convert(tree: CodeTree): CodeTable = {
    def descending_until_leaf(cur_tree: CodeTree, direction: List[Int]): CodeTable = cur_tree match{
      case Leaf(c_leaf, _) => List((c_leaf, direction.reverse))
      case Fork(l, r, _, _) => {
        descending_until_leaf(l,  0 :: direction) ::: descending_until_leaf(r,  1 :: direction)
      }
    }

    descending_until_leaf(tree, List())
  }

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a ::: b

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def quick_encoding(cur_text:List[Char], table: CodeTable, acc_bit_list: List[Bit]): List[Bit] = cur_text match{
      case Nil => acc_bit_list
      case h :: t => quick_encoding(t, table, acc_bit_list ::: codeBits(table)(h))
    }
    
    quick_encoding(text, convert(tree), List())
  }
}
