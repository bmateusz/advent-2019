package hu.bmateusz

import scala.annotation.tailrec

object Day14 {

  /** https://adventofcode.com/2019/day/14 */
  def main(args: Array[String]): Unit = {
    val rules = List(
      List(QC(11, "RVCS")) -> QC(8, "CBMDT"),
      List(QC(29, "QXPB"), QC(8, "QRGRH")) -> QC(8, "LGMKD"),
      List(QC(3, "VPRVD")) -> QC(6, "PMFZG"),
      List(QC(1, "CNWNQ"), QC(11, "MJVXS")) -> QC(6, "SPLM"),
      List(QC(13, "SPDRZ"), QC(13, "PMFZG")) -> QC(2, "BLFM"),
      List(QC(8, "QWPFN")) -> QC(7, "LWVB"),
      List(QC(1, "SPLM")) -> QC(8, "TKWQ"),
      List(QC(2, "QRGRH"), QC(6, "CNWNQ")) -> QC(7, "DTZW"),
      List(QC(2, "DMLT"), QC(1, "SPLM"), QC(1, "TMDK")) -> QC(9, "NKNS"),
      List(QC(1, "MJVXS"), QC(1, "HLBV")) -> QC(7, "PQCQH"),
      List(QC(1, "JZHZP"), QC(9, "LWVB")) -> QC(7, "MJSCQ"),
      List(QC(29, "DGFR")) -> QC(7, "QRGRH"),
      List(QC(14, "XFLKQ"), QC(2, "NKNS"), QC(4, "KMNJF"), QC(3, "MLZGQ"), QC(7, "TKWQ"), QC(24, "WTDW"), QC(11, "CBMDT")) -> QC(4, "GJKX"),
      List(QC(4, "TKWQ"), QC(1, "WLCFR")) -> QC(4, "PDKGT"),
      List(QC(2, "NKNS")) -> QC(4, "GDKL"),
      List(QC(4, "WRZST")) -> QC(9, "XFLKQ"),
      List(QC(19, "DGFR")) -> QC(4, "VPRVD"),
      List(QC(10, "MJSCQ"), QC(4, "QWPFN"), QC(4, "QXPB")) -> QC(2, "MLZGQ"),
      List(QC(1, "JZHZP")) -> QC(7, "QWPFN"),
      List(QC(1, "XFLKQ")) -> QC(9, "FQGVL"),
      List(QC(3, "GQGXC")) -> QC(9, "VHGP"),
      List(QC(3, "NQZTV"), QC(1, "JZHZP")) -> QC(2, "NVZWL"),
      List(QC(38, "WLCFR"), QC(15, "GJKX"), QC(44, "LGMKD"), QC(2, "CBVXG"), QC(2, "GDKL"), QC(77, "FQGVL"), QC(10, "MKRCZ"), QC(29, "WJQD"), QC(33, "BWXGC"), QC(19, "PQCQH"), QC(24, "BKXD")) -> QC(1, "FUEL"),
      List(QC(102, "ORE")) -> QC(5, "DGFR"),
      List(QC(17, "NWKLB"), QC(1, "SBPLK")) -> QC(5, "HRQM"),
      List(QC(3, "BWXGC")) -> QC(8, "TQDP"),
      List(QC(1, "TQDP")) -> QC(2, "PSZDZ"),
      List(QC(2, "MJVXS")) -> QC(9, "WNXG"),
      List(QC(2, "NBTW"), QC(1, "HRQM")) -> QC(2, "SVHBH"),
      List(QC(8, "CNWNQ"), QC(1, "DTZW")) -> QC(4, "RVCS"),
      List(QC(4, "VHGP"), QC(20, "WNXG"), QC(2, "SVHBH")) -> QC(3, "SPDRZ"),
      List(QC(110, "ORE")) -> QC(5, "TXMC"),
      List(QC(10, "QRGRH")) -> QC(5, "NWKLB"),
      List(QC(1, "SBPLK")) -> QC(3, "MJVXS"),
      List(QC(9, "DGFR")) -> QC(5, "RFSRL"),
      List(QC(5, "LBTV")) -> QC(3, "DMLT"),
      List(QC(1, "NWKLB"), QC(1, "KMNJF"), QC(1, "HDQXB"), QC(6, "LBTV"), QC(2, "PSZDZ"), QC(34, "PMFZG"), QC(2, "SVHBH")) -> QC(2, "WJQD"),
      List(QC(1, "RVCS")) -> QC(5, "MKRCZ"),
      List(QC(14, "NQZTV"), QC(3, "FPLT"), QC(1, "SJMS")) -> QC(2, "GQGXC"),
      List(QC(18, "RFSRL"), QC(13, "VHGP"), QC(23, "NBTW")) -> QC(5, "WTDW"),
      List(QC(1, "VHGP"), QC(6, "TKWQ")) -> QC(7, "QXPB"),
      List(QC(1, "JZHZP"), QC(1, "CNWNQ")) -> QC(5, "KMNJF"),
      List(QC(109, "ORE")) -> QC(9, "BWXGC"),
      List(QC(2, "CNWNQ"), QC(1, "PDKGT"), QC(2, "KMNJF")) -> QC(5, "HDQXB"),
      List(QC(1, "PDKGT"), QC(18, "WRZST"), QC(9, "MJSCQ"), QC(3, "VHGP"), QC(1, "BLFM"), QC(1, "LGMKD"), QC(7, "WLCFR")) -> QC(2, "BKXD"),
      List(QC(11, "MLJK")) -> QC(6, "FPLT"),
      List(QC(8, "DGFR"), QC(2, "TXMC"), QC(3, "WJRC")) -> QC(9, "SJMS"),
      List(QC(2, "SBPLK")) -> QC(1, "LBTV"),
      List(QC(22, "QWPFN")) -> QC(4, "WRZST"),
      List(QC(5, "WRZST"), QC(22, "WNXG"), QC(1, "VHGP")) -> QC(7, "NBTW"),
      List(QC(7, "RVCS")) -> QC(9, "TMDK"),
      List(QC(1, "DGFR"), QC(14, "TXMC")) -> QC(5, "JZHZP"),
      List(QC(2, "JZHZP")) -> QC(3, "SBPLK"),
      List(QC(19, "PDKGT")) -> QC(8, "HLBV"),
      List(QC(195, "ORE")) -> QC(6, "WJRC"),
      List(QC(6, "GQGXC")) -> QC(8, "CNWNQ"),
      List(QC(1, "NVZWL"), QC(4, "GQGXC")) -> QC(2, "CBVXG"),
      List(QC(1, "NVZWL"), QC(1, "KMNJF")) -> QC(8, "WLCFR"),
      List(QC(153, "ORE")) -> QC(4, "MLJK"),
      List(QC(1, "BWXGC")) -> QC(6, "NQZTV")
    )

    println(inverseApplyRules(rules, Map(fuel -> 1))(ore))
    println(findTrillion(rules, 1, 100000))
  }

  private val ore = "ORE"
  private val fuel = "FUEL"
  private val trillion = 1000000000000L

  case class QC(n: Long, chemical: String) {
    def matches(other: QC): Boolean = {
      chemical == other.chemical && n >= other.n
    }

    def addToMap(map: Map[String, Long], multiplier: Long): Map[String, Long] = {
      map + (chemical -> (map.getOrElse(chemical, 0L) + n * multiplier))
    }

    def subFromMap(map: Map[String, Long], multiplier: Long): Map[String, Long] = {
      map + (chemical -> (map(chemical) - n * multiplier))
    }
  }

  @tailrec
  def inverseApplyRules(rules: List[(List[QC], QC)], materials: Map[String, Long]): Map[String, Long] = {
    rules.find { case (_, to) =>
      materials.get(to.chemical).exists(_ > 0)
    } match {
      case Some((from, to)) =>
        val times = Math.max(materials(to.chemical) / to.n, 1)
        inverseApplyRules(
          rules,
          from.foldLeft(to.subFromMap(materials, times)) {
            case (acc, curr) => curr.addToMap(acc, times)
          }
        )
      case None =>
        materials.filterNot(_._2 == 0)
    }
  }

  @tailrec
  def findTrillion(rules: List[(List[QC], QC)], fuels: Long, step: Long): Long = {
    val result = tryTrillion(rules, fuels)
    if ((result == -1 && step > 0) || (result == 1 && step < 0)) {
      findTrillion(rules, fuels + step, step)
    } else if ((result == -1 && step < 0) || (result == 1 && step > 0)) {
      val newStep = step / 10 * -1
      findTrillion(rules, fuels + newStep, newStep)
    } else if (result == 0) {
      fuels
    } else {
      throw new IllegalArgumentException(s"wrong arguments $fuels $step $result")
    }
  }

  def tryTrillion(rules: List[(List[QC], QC)], fuels: Long): Int = {
    val one = inverseApplyRules(rules, Map(fuel -> fuels))(ore)
    val plusOne = inverseApplyRules(rules, Map(fuel -> (fuels + 1L)))(ore)
    if (plusOne < trillion) {
      -1
    } else if (one > trillion) {
      1
    } else {
      0
    }
  }

}
