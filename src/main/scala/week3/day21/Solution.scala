package week3.day21

object Solution {

  def main(args: Array[String]): Unit = {
    val ingredientsWithAllergens = utils.loadInputAsListOfStrings("week3/day21/input.txt").map { line =>
      val (ingr: String) :: (allerg: String) :: Nil = line.split("\\(contains").toList
      (
        ingr.trim().split(" ").toSet,
        allerg
          .replace(")", "")
          .replace(",", "")
          .trim()
          .split(" ")
          .toSet
      )
    }

    utils.timeSolution("Part 1", () => solvePartOne(ingredientsWithAllergens))
    utils.timeSolution("Part 2", () => solvePartTwo(ingredientsWithAllergens))
  }

  private def solvePartOne(ingredientsWithAllergens: List[(Set[String], Set[String])]): Int = {
    val allergenMappings = mapIngredientsToAllergens(ingredientsWithAllergens)

    ingredientsWithAllergens.flatMap(_._1).count { ingredient =>
      !allergenMappings.keys.toSet.contains(ingredient)
    }
  }

  private def mapIngredientsToAllergens(ingredientsWithAllergens: List[(Set[String], Set[String])]) = {
    val allergens = ingredientsWithAllergens.flatMap(_._2).distinct

    val possibleMatches = allergens.map { allergen =>
      val matchingIngredientLists = ingredientsWithAllergens.collect {
        case (ingredients, allergens) if allergens.contains(allergen) => ingredients
      }
      val possibleMatchingIngredients = matchingIngredientLists.tail.foldLeft(matchingIngredientLists.head) {
        case (ingredientsIntersectionAcc, ingredients) =>
          ingredientsIntersectionAcc.intersect(ingredients)
      }
      (allergen, possibleMatchingIngredients)
    }

    val (discovered, undecided) = possibleMatches
      .sortBy(_._2.size)
      .foldLeft((Map.empty[String, String], Map.empty[String, Set[String]])) {
        case ((discoveredMatches, undecided), (allergen, possibleMatchingIngredients)) =>
          val matched = possibleMatchingIngredients.diff(discoveredMatches.keys.toSet)
          val newDiscovered =
            if (matched.size == 1) discoveredMatches + (matched.head -> allergen) else discoveredMatches
          val newUndecided = if (matched.size > 1) undecided + (allergen -> matched) else undecided
          (newDiscovered, newUndecided)
      }

    undecided.toList.sortBy(_._2.size).foldLeft(discovered) {
      case (discoveredMatches, (allergen, possibleMatchingIngredients)) =>
        discoveredMatches + (possibleMatchingIngredients.diff(discoveredMatches.keys.toSet).head -> allergen)
    }
  }

  private def solvePartTwo(ingredientsWithAllergens: List[(Set[String], Set[String])]): String = {
    mapIngredientsToAllergens(ingredientsWithAllergens).toList.sortBy(_._2).map(_._1).mkString(",")
  }

}
