package exercises

import util.DayN

object Day21 extends DayN {
  override val num = 21

  val regex = """(.*) \(contains (.*)\)""".r

  case class Ingredient(value: String)
  case class Allergen(value: String)
  case class Recipe(ingredients: Set[Ingredient], allergens: Set[Allergen])

  def parse(lines: List[String]): List[Recipe] = lines.map { case regex(ingredientString, allergenString) => Recipe(
    ingredientString.split(" ").map(Ingredient.apply).toSet,
    allergenString.split(", ").map(Allergen.apply).toSet
  )}

  def possibleAllergens(ingredient: Ingredient, recipes: List[Recipe]): Set[Allergen] = {
    val ingredientRecipes = recipes.filter(_.ingredients.contains(ingredient))
    def allergenRecipes(a: Allergen) = recipes.filter(_.allergens.contains(a))
    val possibleAllergens = ingredientRecipes.flatMap(_.allergens).toSet
    possibleAllergens.filter(a => allergenRecipes(a).forall(_.ingredients.contains(ingredient)))
  }

  def safeIngredients(recipes: List[Recipe]): Set[Ingredient] =
    recipes.flatMap(_.ingredients).toSet.filterNot(i => possibleAllergens(i, recipes).nonEmpty)

  def ingredientsWithAllergens(ingredientOptions: List[Ingredient], recipes: List[Recipe]): List[(Ingredient, Allergen)] = {
    def loop(options: Map[Ingredient, Set[Allergen]]): Map[Ingredient, Set[Allergen]] = {
      val foundAllergens = options.filter(_._2.size == 1).values.map(_.head).toSet
      val newOptions = options.map {
        case (k, v) if v.size > 1 => k -> v.diff(foundAllergens)
        case (k, v) => k -> v
      }
      if (newOptions == options) options
      else loop(newOptions)
    }

    val initialOptions = ingredientOptions.map(i => i -> possibleAllergens(i, recipes)).toMap
    val result = loop(initialOptions)
    result.map { case (k, v) => k -> v.head }.toList
  }

  val recipes = parse(lines)

  part1(safeIngredients(recipes).toList.map(i => recipes.count(_.ingredients.contains(i))).sum)

  part2 {
    ingredientsWithAllergens(recipes.flatMap(_.ingredients).toSet.diff(safeIngredients(recipes)).toList, recipes)
      .sortBy(_._2.value)
      .map(_._1.value)
      .mkString(",")
  }
}
