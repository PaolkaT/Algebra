import java.io.{File, PrintWriter}

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
    // вывод в файл
    writeInFile("table_task_1.csv", finalString)
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
    // вывод в файл
    writeInFile("table_task_2.csv", finalString)
  }

  def task_3(a: String, n: Int, k: Int) = {
    val alphabet = "абвгдеёжзийклмнопрстуфхцчшщъыьэюя!@#$%^&*(),.?-1234567890 "
    val procText = a.toLowerCase
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
    print(decod)
    println("Декодирование:")
    val decodChar = decod.map(x => alphabet.charAt(x)).mkString
    println(decodChar)
  }

  def binToDec(string: String): Int = {
    Integer.parseInt(string, 2)
  }

  def task_5() = {
    // отладочный ключ
    val key = "101010"
    // алфавит и его кодирование
    val alphabet = "абвгдеёжзийклмнопрстуфхцчшщъыьэюя!(),.?-1234567890 "
    // (0.toString * (6 - alphabet.indexOf(x).toBinaryString.length)) нужно для добавления нулей слева
    val binaryCodes = alphabet
      .map(x => (0.toString * (6 - alphabet.indexOf(x).toBinaryString.length)) + alphabet.indexOf(x).toBinaryString)
    // чтение из файла
    val readFromFile = Source.fromFile("text_for_task")
    val stringForProcess = readFromFile.mkString.toLowerCase
    // соответствие букв и кода
    val codeInputText = stringForProcess.map(x => binaryCodes(alphabet.indexOf(x)))
    println(codeInputText)
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

  def main(args: Array[String]): Unit = {
//    task_1()
//    task_2()
    task_5()
//    task_3("Я зашифровал здесь что-то", 1, 2)
//    task_3("Я зашифровал здесь что-то", 1, 2)
  }
}
