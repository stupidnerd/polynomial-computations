# polynomial-computation

Программа для алгебраических вычислений - M

Объекты, с которыми вы будете работать, - это многочлены от нескольких переменных, представленных в символьном виде с вещественными коэфициентами. Многочлены должны изображаться как арифметические выражения, так умножение изображается знаком *, а возведение в степень знаком ^. Для манипуляции с многочленами нужны некоторые команды, чтобы пользователь мог получить ответы на вопросы, на которые не удается ответить с помощью традиционных языков программирования. Для этого вам понадобится обозначать многочлены идентификаторами. Команды выполняют некоторые операции над своими операндами и помещают результат в качестве значения некоторого имени многочлена.
Список команд:
1. Ввести многочлен и записать его под некоторым именем.
2. Образовать алгебраическую сумму (разность, произведение) двух многочленов и записать полученный многочлен под некоторым именем.
3. Возвести данный многочлен в целую степень и результат записать под некоторым именем.
4. Заменить каждое вхождение некоторой переменной в многочлене на данный многочлен и результат записать под некоторым именем.
5. Вычислить производную многочлена по переменной и результат записать под некоторым именем.
6. Напечатать данный многочлен.
Многочлен представлять в виде суммы членов, включающих только операции умножения и возведения в степень. В каждом таком одночлене все константы перемножены и образуют числовой коэфициент (первый сомножитель), переменные упорядочены по алфавиту и все степени одной переменной объеденены так, что каждая переменная встречается лишь один раз. Следует приводить подобные члены, т.е. объединять одночлены, имеющие одинаковые наборы переменных и степеней, с соответствующим изменением коэффициентов. Для представления многочленов в памяти используйте списковые структуры.