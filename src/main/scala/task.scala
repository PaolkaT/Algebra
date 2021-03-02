import java.io.{File, PrintWriter}

import sun.security.util.Length

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.io.Source

object task {
  def checkChar(x: Char): Boolean = {
    val chars = "абвгдеёжзийклмнопрстуфхцчшщъыьэюя"
    if (chars.contains(x) || chars.toUpperCase.contains(x)) {
      return true
    }
    false
  }

  def writeInFile(path: String, stringForWrite: Map[_, Double]) = {
    val writer = new PrintWriter(new File(path))
    writer
      .write(stringForWrite.toSeq.mkString.replace(')', '\n').replaceAll("[(]", ""))
    writer.close()
  }

  def task_1() = {
    val readFromFile = Source.fromFile("text_for_task")
    val stringForProcess = readFromFile.mkString
    // обработка строки
    val filteredString = stringForProcess.toLowerCase.filter(x => checkChar(x))
    val groupedString = filteredString.groupBy(identity).view.mapValues(_.length).toMap
    val len = filteredString.length
    // подсчёт частоты
    val finalString = groupedString
      .map(x => x._1 -> BigDecimal(x._2.toDouble / len.toDouble).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble)
      .filter(x => x._2 != 0.000)
    // для красивого вывода
    val sortedMap = ListMap(finalString.toSeq.sortWith(_._2 > _._2):_*)
    // вывод в файл
    writeInFile("table_task_1.csv", sortedMap)
  }

  def task_2() = {
    val readFromFile = Source.fromFile("text_for_task")
    val stringForProcess = readFromFile.mkString
    // обработка строки
    val filteredString = stringForProcess.toLowerCase.filter(x => checkChar(x)) //.groupBy(identity).view.mapValues(_.length).toMap
    // цикл для нахождения пар подряд идущих элементов
    var allPairs = List((filteredString(0), filteredString(1)))
    for (i <- 1 to filteredString.length - 2) {
      allPairs = (filteredString(i), filteredString(i + 1)) :: allPairs
    }
    val ident = allPairs.groupBy(identity).view.mapValues(_.length).toMap
    val len = filteredString.length
    // подсчёт частоты
    val finalString = ident
      .map(x => x._1._1.toString + x._1._2.toString ->
        BigDecimal(x._2.toDouble / len.toDouble).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble)
      .filter(x => x._2 != 0.000)
    // для красивого вывода
    val sortedMap = ListMap(finalString.toSeq.sortWith(_._2 > _._2):_*)
    // вывод в файл
    writeInFile("table_task_2.csv", sortedMap)
  }

  def task_3(n: Int, k: Int) = {
    val alphabet = "абвгдеёжзийклмнопрстуфхцчшщъыьэюя!@#$%^&*(),.?-1234567890 \n"
    val readFromFile = Source.fromFile("text_for_task")
    val stringForProcess = readFromFile.mkString
    val procText = stringForProcess.toLowerCase
    val m = alphabet.length // length
    // кодирование
    val cod = procText.map(x => (alphabet.indexOf(x) * n + k) % m) // .map(x => b((m + x) % m))
    println("Кодирование:")
    val codChar = cod.map(x => alphabet.charAt(x)).mkString
    println(codChar)
    // декодирование
    val decod = codChar
      .map(x =>
        if (alphabet.indexOf(x) - k >= 0) (alphabet.indexOf(x) - k) % m
        else (m + alphabet.indexOf(x) - k) % m)
    //    print(decod)
    println("Декодирование:")
    val decodChar = decod.map(x => alphabet.charAt(x)).mkString
    println(decodChar)
  }

  @tailrec
  def makeTable(count: Int = 32, tableVigenere: List[String] = List(),
                alphabet: String = "абвгдеёжзийклмнопрстуфхцчшщъыьэюя"): List[String] = tableVigenere match {
    case tbl =>
      if (count >= 0) {
        // что же это такое? первая мапа кодирует со сдвигом 1. Вторая мапа меняет позиции в алфавите на буквы
        makeTable(count - 1,
          alphabet
            .map(x => (alphabet.indexOf(x) + count) % alphabet.length).map(x => alphabet.charAt(x)).mkString :: tbl)
      }
      else tbl
  }

  def task_4(keyword: String) = {
    val alphabet = "абвгдеёжзийклмнопрстуфхцчшщъыьэюя!@#$%^&*(),.?-1234567890 \n"

    val tabulaRecta = makeTable()

    val readFromFile = Source.fromFile("text_for_task")
    val stringForProcess = readFromFile.mkString.toLowerCase
    val charToIndexCod = stringForProcess.map(x => alphabet.indexOf(x))
    val cod = charToIndexCod.zipWithIndex.map{ case (x, i) =>
      if (x <= 32) tabulaRecta(x).charAt(alphabet.indexOf(keyword.charAt(i % keyword.length)))
      else alphabet.charAt(x)}
    println("Зашифрованное сообщение: " + cod.mkString)

    val charToIndexDecod = cod.map(x => alphabet.indexOf(x))
    val decod = charToIndexDecod.zipWithIndex.map{ case (x, i) =>
      if (x <= 32) {
        tabulaRecta(alphabet.indexOf(keyword.charAt(i % keyword.length))).indexOf(alphabet.charAt(x))
      }
      else x}
    val decodChar = decod.map(x => alphabet.charAt(x))
    println("Декодированное сообщение: " + decodChar.mkString)
  }

  def binToDec(string: String): Int = {
    Integer.parseInt(string, 2)
  }

  def task_5(key: String) = {
    // алфавит и его кодирование
    val alphabet = "абвгдеёжзийклмнопрстуфхцчшщъыьэюя!(),.?-1234567890 \n"
    // (0.toString * (6 - alphabet.indexOf(x).toBinaryString.length)) нужно для добавления нулей слева
    val binaryCodes = alphabet
      .map(x => (0.toString * (6 - alphabet.indexOf(x).toBinaryString.length)) + alphabet.indexOf(x).toBinaryString)
    // чтение из файла
    val readFromFile = Source.fromFile("text_for_task")
    val stringForProcess = readFromFile.mkString.toLowerCase
    // соответствие букв и кода
    val codeInputText = stringForProcess.map(x => binaryCodes(alphabet.indexOf(x)))
    // непосредственно xor с ключом
    val xor = codeInputText.map(x => (binToDec(x) ^ binToDec(key)).toBinaryString)
    val xorChars = xor.map(x => alphabet.charAt(binToDec(x) % alphabet.length))
    println("Сообщение после кодирования: " + xorChars.mkString)
    // декодирование
    val decodCode = xor
      .map(x =>
        (0.toString * (6 - (binToDec(x) ^ binToDec(key)).toBinaryString.length))
          + (binToDec(x) ^ binToDec(key)).toBinaryString)
    val decodChars = decodCode.map(x => alphabet.charAt(binToDec(x)))
    println("Сообщение после декодирования: " + decodChars.mkString)
  }

  def generateKey(keyword: List[Int], output: String = "")
                 (count: Int = 0,
                  length: Int = (scala.math.pow(2, keyword.length) - 1).toInt): (List[Int], String) = keyword match {
    case head :: tail =>
      if (count < length) {
        val newElem = head ^ tail.last
        generateKey(newElem :: (List(head) ::: tail.init), output + tail.last.toString)(count + 1)
      }
      else {
        (keyword, output)
      }
    case _ => (keyword, output)
  }

  def task_6(keyword: List[Int]) = {
    val key = generateKey(keyword)()._2
    task_5(key)
  }

  def main(args: Array[String]): Unit = {
//    task_1()
//    task_2()
//    task_3(1, 3)
//    task_4("тест")
//    task_5("101010")
    task_6(List(0, 0, 1))
  }
}