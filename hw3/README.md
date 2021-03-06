# Haskell: ДЗ 3 -- Вычисления с состоянием

Данное домашнее задание нацелено на обучение навыкам использования более сложных и мощных монад (`Writer`, `Reader`, `State`, `IO`), а также трансформеров монад. Умение использовать эти инструменты крайне необходимо для написания больших и полноценных приложений.

В этом задании необходимо написать интерпретатор очень простого императивного языка. Это большое и сложное задание разбито на части, которые можно выполнять независимо, а затем необходимо объединить в одну большую программу.

В данном домашнем задании Вам необходимо использовать следующие библиотеки:

* `containers` -- для отображения из имён переменных в значения;
* `megaparsec` -- для парсинга;
* `mtl` -- для монад `State`, `Reader` и трансформеров монад.
 
## Часть 1: Арифметические выражения

Создайте тип данных `Expr`, обозначающий арифметические выражения, в котором можно задавать:

1. Константы.
2. Переменные.
3. Сложение двух выражений.
4. Вычитание двух выражений.
5. Умножение двух выражений.
6. Целочисленное деление двух выражений.
7. Локальное объявление иммутабельной переменной внутри выражения.

Например, следующее выражение в Haskell:

```haskell=
Var "x" `Add` (Lit 3 `Mul` ("x" `Let` (Lit 2) $ Var "x"))
```

соответствует такому арифметическому выражению в языке:

```
x + 3 * (let x = 2 in x)
```

а его вычисление при изначальном `x = 1` должно быть равно `7`.

Далее напишите функцию, используя монаду `Reader`, которая вычисляет переданное выражение и возвращает результат вычисления или же ошибку, если не получилось вычислить. Для просты рекомендую сначала написать без монады `Reader` и без обработки ошибок, затем добавить `Reader`, а затем уже добавить обработку ошибок.

## Часть 2: Парсер арифметических выражений

Используя библиотеку [`megaparsec`](https://hackage.haskell.org/package/megaparsec) напишите парсер типа данных `Expr`. Используйте стандартные возможности этой библиотеки, чтобы парсер получился простым и лаконичным.

> Объявление локальной иммутабельной переменной через `let` всегда берётся в скобки.

## Часть 3: Обновление переменных

Обновление множества переменных в языке возможно двумя способами:

1. Создание новой мутабельной переменной.
2. Присваивание существующей мутабельной переменной нового значения.

В языке это записывается следующим образом:

```haskell
mut x = 3 + 4  -- объявление переменной
x = x * 5      -- обновление переменной
```

Напишите две функции, которые ответственны за создание и обновление переменных, используя монаду `State`. При этом учтите следующее:

1. Нельзя повторно объявлять переменную с таким же именем.
2. Нельзя присвоить новое значение необъявленной переменной.

Эти функции должны принимать **не** `Expr`, а целое число. Вычисление выражений будет в следующих заданиях. Аналогичные советы для последовательной реализации этой функции применяются как в задании для `Reader`.

## Часть 4: Парсер создания переменных

Придумайте типы данных для объявления и обновления переменных. После чего напишите парсер этих двух конструкций языка.

## Часть 5: Полноценное присваивание

Используя написанные ранее функции, напишите функцию, которая принимает список конструкций языка (в данный момент пока только создание и присваивание переменных), вычисляет выражение справа от `=`, а затем обновляет текущее множество переменных согласно результату вычисления. Если вычислить не удалось, то нужно завершить интерпретацию и показать, где именно возникла ошибка и какая.

## Часть 6: Вывод в консоль

Добавим новую конструкцию в наш язык: _вывод результата выражения в консоль_.

В языке это будет записываться следующим образом:

```
< x + 3  -- вывод результата (x + 3) в консоль
```

По аналогии с предыдущими заданиями: придумайте тип данных для этой конструкции, напишите парсер и добавьте этот случай в обработку.

## Часть 7: Ввод из консоли

А теперь необходимо добавить считывание одного числа из консоли в переменную. В языке это записывается как:

```
> x
```

## Часть 8*: Циклы со счётчиком

Добавьте в язык программирования цикл `for`, который принимает начальное значение счётчика и конечное, увеличивая счётчик на `1`.
После добавления этой конструкции в вашем языке должно стать возможным вычисление факториала и чисел Фибоначчи.

## Часть 9*: Прерывания цикла

Добавьте в язык ключевое слово `break`, которое будет завершать текущий цикл. Конечно, без условного оператора в данном слове мало смысла. Но для упражнения важно научиться использовать язык программирования Haskell. Поэтому данную конструкцию необходимо реализовать при помощи `MonadCont`.

## Часть 10: Запуск программы

Наконец, интерпретатор необходимо запустить. Программа на интерпретируем языке находится в файле. Аргументом командной строки вашему интерпретатору передаётся путь к файлу, из которого надо считать программу.

## Прочие замечания

Когда Вы используете трансформеры монад, то обычно принято создавать свой собственный `newtype`, являющийся обёрткой над вашим большим контекстом. А функции писать как можно более полиморфные с использованием классов типов `MonadState, MonadReader, MonadIO`. Это будет бонусом, если Вы сможете соблюсти такой стиль реализации. 