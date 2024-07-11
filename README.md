Почему сотрудники увольняются?
================
Помазкова Арина

## Задача

Задача – **проанализировать отток сотрудников из компании**. 3аказчику,
то есть работодателю (в частности, его руководству и HR-отделу),
необходимо выявить свои слабые стороны, чтобы понять, почему работники
не хотят продолжать с ним сотрудничать. Эта аналитика может быть
необходима, чтобы:

1)  Уменьшить «текучку кадров»;
2)  Сократить убытки от ухода ценных работников;
3)  Улучшать условия труда для привлечения новых сотрудников;
4)  В долгосрочной перспективе сформировать репутацию надёжного
    работодателя и т.д.

Для выполнения этой цели заказчику необходимы:

1)  Подробная аналитка основных причин оттока сотрудников;
2)  Выявление «групп риска», наиболее склонных к уходу из компании;
3)  Представление возможных решений и изменений в соответствии с
    причинами оттока.

Именно эти задачи решает данная работа.

## Анализ

### Данные и логика анализа

Посмотрим на распределение оттока по возрасту сотрудников.

``` r
library(dplyr)
library(knitr)
library(kableExtra)

data3 = dbGetQuery(con, "SELECT Attrition, Age,
                        CASE 
                            WHEN Age > 35 THEN 'old'
                            ELSE 'young'
                            END AS group
                        FROM portfolio 
                        INNER JOIN profile USING(EmployeeNumber)")

library(ggplot2)
  
ggplot(data3) +
  geom_col(aes(x = Age, y = sum(Attrition))) +
  labs(title = 'Распределение оттока по возрастам среди сотрудников') +
  xlab('Возраст') +
  ylab('Количество уволившихся') +
  theme_minimal()
```

![image](https://github.com/pomazarin/hr-analysis/assets/168182782/5f9502ed-307f-4a36-98e9-01aebf152b18)

Наиболее интересным мне показался **сегмент молодых работников до 35 лет
включительно** (эту возрастную группу принято считать молодёжью). В
процентном соотношении эта подгруппа чаще уходит из компании: 22.8%
молодых специалистов покидают компанию по сравнению с 11.5% работников,
старше 35 лет. Интересно посмотреть, с чем связан более активный отток
молодых кадров.

``` r
#разведка
data1 = dbGetQuery(con, "SELECT Attrition, 
                        ROUND(COUNT(*)/(SELECT COUNT(*) FROM portfolio 
                                  INNER JOIN profile USING(EmployeeNumber)
                                  WHERE Age <=35)*100, 2) as prop 
                        FROM portfolio 
                        INNER JOIN profile USING(EmployeeNumber)
                        WHERE Age <= 35
                        GROUP BY Attrition")

data2 = dbGetQuery(con, "SELECT Attrition, 
                        ROUND(COUNT(*)/(SELECT COUNT(*) FROM portfolio 
                                  INNER JOIN profile USING(EmployeeNumber)
                                  WHERE Age <=35)*100, 2) as prop 
                        FROM portfolio 
                        INNER JOIN profile USING(EmployeeNumber)
                        WHERE Age > 35
                        GROUP BY Attrition")

knitr::kable(data1, align = "ccc", caption = 'Процент ушедших и оставшихся сотрудников до 35 лет') %>% kable_classic(full_width = T, html_font = "Cambria") %>% kableExtra::kable_minimal() 
```

<table class=" lightable-classic lightable-minimal" style="font-family: Cambria; margin-left: auto; margin-right: auto; font-family: &quot;Trebuchet MS&quot;, verdana, sans-serif; margin-left: auto; margin-right: auto;">
<caption>
Процент ушедших и оставшихся сотрудников до 35 лет
</caption>
<thead>
<tr>
<th style="text-align:center;">
Attrition
</th>
<th style="text-align:center;">
prop
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;">
0
</td>
<td style="text-align:center;">
77.25
</td>
</tr>
<tr>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
22.75
</td>
</tr>
</tbody>
</table>

``` r
knitr::kable(data2, align = "ccc", caption = 'Процент ушедших и оставшихся сотрудников старше 35 лет') %>% kable_classic(full_width = T, html_font = "Cambria") %>% kableExtra::kable_minimal()
```

<table class=" lightable-classic lightable-minimal" style="font-family: Cambria; margin-left: auto; margin-right: auto; font-family: &quot;Trebuchet MS&quot;, verdana, sans-serif; margin-left: auto; margin-right: auto;">
<caption>
Процент ушедших и оставшихся сотрудников старше 35 лет
</caption>
<thead>
<tr>
<th style="text-align:center;">
Attrition
</th>
<th style="text-align:center;">
prop
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;">
0
</td>
<td style="text-align:center;">
90.82
</td>
</tr>
<tr>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
11.47
</td>
</tr>
</tbody>
</table>

В финальном датасете для построения предсказательной модели оставляем
только работников до 35 лет включительно. Для предсказания будут
использованы 9 переменных: демографические показатели (пол и семейное
положение), все переменные, связанные с удовлетворённостью работой (в
том числе коллективом и рабочей средой), показатели переработки
(work-life balance и переработка), а также все переменные, связанные с
оплатой труда (годовой доход и повышение зарплаты). Предполагается, что
именно эти факторы влияют на отток молодых сотрудников из компании.

Дополнительно в финальном датасете были преобразованы все номинальные
переменные в категориальные.

``` r
data = dbGetQuery(con, 'SELECT Gender, MaritalStatus, WorkLifeBalance,
                        Attrition, EnvironmentSatisfaction,
                        JobSatisfaction, MonthlyIncome,
                        OverTime, PercentSalaryHike, RelationshipSatisfaction
                        FROM portfolio 
                        INNER JOIN profile USING(EmployeeNumber)
                        WHERE Age <= 35')

data = data %>% mutate_if(is.character, as.factor)
data$Attrition = data$Attrition %>% as.factor()
```

### Модель

Так как перед нами стоит задача классификации, то для предсказания
напрашиваются два метода: логистическая регрессия и дерево решений.
Посмотрим на показатели качества двух моделей. Учтём, что в данном
случае нам особенно важен показатель specificity, так как наши данные
несбалансированы и в датасете гораздо больше случаев, когда работники не
уходили из компании.

Cначала смотрим показатели качества модели дерева решений.

``` r
library(tidymodels)
library(caret)
library(rsample)

set.seed(134)
Split = initial_split(data, prop = 0.8)
train = training(Split)
test = testing(Split)

#дерево решений
treemodel = decision_tree(mode = 'classification', engine = 'rpart')
set.seed(134)
tree = treemodel %>% fit(Attrition~., data = train)
predtree = predict(tree, test)

confusionMatrix(predtree$.pred_class, test$Attrition)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  0  1
    ##          0 71 17
    ##          1  7 10
    ##                                           
    ##                Accuracy : 0.7714          
    ##                  95% CI : (0.6793, 0.8477)
    ##     No Information Rate : 0.7429          
    ##     P-Value [Acc > NIR] : 0.29277         
    ##                                           
    ##                   Kappa : 0.3193          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.06619         
    ##                                           
    ##             Sensitivity : 0.9103          
    ##             Specificity : 0.3704          
    ##          Pos Pred Value : 0.8068          
    ##          Neg Pred Value : 0.5882          
    ##              Prevalence : 0.7429          
    ##          Detection Rate : 0.6762          
    ##    Detection Prevalence : 0.8381          
    ##       Balanced Accuracy : 0.6403          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

``` r
#accuracy = 0.781 spec = 0.4
```

Теперь логистической регрессии.

``` r
#логичтическая регрессия
model = logistic_reg()
set.seed(134)
logreg = model %>% fit(Attrition~., data = train)
predlog = predict(logreg, test)

confusionMatrix(predlog$.pred_class, test$Attrition)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  0  1
    ##          0 73 13
    ##          1  5 14
    ##                                           
    ##                Accuracy : 0.8286          
    ##                  95% CI : (0.7427, 0.8951)
    ##     No Information Rate : 0.7429          
    ##     P-Value [Acc > NIR] : 0.02525         
    ##                                           
    ##                   Kappa : 0.5032          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.09896         
    ##                                           
    ##             Sensitivity : 0.9359          
    ##             Specificity : 0.5185          
    ##          Pos Pred Value : 0.8488          
    ##          Neg Pred Value : 0.7368          
    ##              Prevalence : 0.7429          
    ##          Detection Rate : 0.6952          
    ##    Detection Prevalence : 0.8190          
    ##       Balanced Accuracy : 0.7272          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

``` r
#accuracy = 0.8286 spec= 0.45
```

Итак, по результатам построения первичных моделей показатели первой
оказались хуже, чем у второй (accuracy = 0.781 и specificity = 0.4,
accuracy = 0.8286 и specificity = 0.45 соответсвенно). Поэтому выбираем
**модель логистической регрессии**.

Постараемся улучшить specificity с помощью обучения модели на данных с
дублирующимися наблюдениями из малочисленного класса. **Используем
up-sampling**.

``` r
set.seed(134)
ds_up <- recipe(~., data = train) %>%
  themis::step_upsample(Attrition) %>% 
  prep(training = train, retain = TRUE) %>% 
  bake(new_data = NULL)

set.seed(134)
up_log_model = model %>% fit(Attrition ~., ds_up)

pred_up = predict(up_log_model, test)

confusionMatrix(pred_up$.pred_class, test$Attrition)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  0  1
    ##          0 58  6
    ##          1 20 21
    ##                                           
    ##                Accuracy : 0.7524          
    ##                  95% CI : (0.6586, 0.8314)
    ##     No Information Rate : 0.7429          
    ##     P-Value [Acc > NIR] : 0.46273         
    ##                                           
    ##                   Kappa : 0.4458          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.01079         
    ##                                           
    ##             Sensitivity : 0.7436          
    ##             Specificity : 0.7778          
    ##          Pos Pred Value : 0.9062          
    ##          Neg Pred Value : 0.5122          
    ##              Prevalence : 0.7429          
    ##          Detection Rate : 0.5524          
    ##    Detection Prevalence : 0.6095          
    ##       Balanced Accuracy : 0.7607          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

``` r
#accuracy = 0.76 spec= 0.9
```

Несмотря на то, что показатель accuracy уменьшился, с помощью
up-sampling мы значительно улучшили specificity.

Посмотрим, какие переменные в предсказании имеют большую значимость.

``` r
library(vip)
vip(up_log_model)
```

![image](https://github.com/pomazarin/hr-analysis/assets/168182782/46af0f1b-9e7a-474b-aeec-971ac31f6cf9)

Итак, можно заметить, что самый значимый фактор увольнения молодых
сотрудников – это **переработка**. Также важна **удовлетворённость
рабочей средой** и **годовой доход**. Примечательно, что среди
уволившихся много тех, кто очень доволен своей работой и work-life
balance. На эти факторы компания, к сожалению, повлиять не может, так
как, казалось бы, эти показатели и так уже приняли наилучшее для
работников значение. Также странно предлагать компании повысить
зарплату, так как, во-первых, у нас недостаточно данных, чтобы
определить, насколько нужно её повысить, а во-вторых, скорее всего, для
всех сотрудников такой обширной возрастной группы сложно подобрать
фиксированное и универсальное значение изменения оплаты труда. Влияние
заработной платы на увольнение работников может претендовать на
отдельное исследование.

### Симуляция

Так как на некоторые важные факторы оттока молодых сотрудников компания
повлиять не может (в том числе на семейный статус), я предлагаю повлиять
на **самые важные показатели – переработку и неудовлетворённость рабочей
средой**. Предположительно, количество ушедших молодых сотрудников
должно уменьшиться, если переработки станет меньше, а удовлетворение
рабочей обстановкой повысится.

Смоделируем ситуацию, в которой эти показатели изменились. Предположим,
что *в 30% случаев работники перестали перерабатывать*, например:

1)  После введения корпоративного запрета на пребывание на рабочем месте
    при достижении определённого количества сверхурочных часов;

2)  При регулярном мониторинге состояния работников из группы риска и
    оказании необходимой психологической помощи перерабатывающим
    работникам;

3)  При введении обязательного денежного вознаграждения за переработку и
    т. д.

Также предположим, что *в 30% случаях удовлетворённость рабочей средой
повысилась* (в 20% с низкой на среднюю, в 10% с низкой на высокую).
Такая ситуация возможна, например:

1)  После капитального ремонта офиса;

2)  При развитии зон отдыха на рабочем месте;

3)  После обновлении рабочей техники и т. д.

Посмотрим, насколько сократилось количество уволившихся молодых
сотрудников (в процентах) при **возможном** введении предложенных
изменений.

``` r
test2 = test
set.seed(134)
test2$OverTime[test2$OverTime == "Yes"] = 
  sample(c("Yes", "No"), 
         size = length(test2$OverTime[test2$OverTime == "No"]),
         replace = T, prob = c(0.7, 0.3))

set.seed(134)
test2$EnvironmentSatisfaction[test2$EnvironmentSatisfaction == "Low"] = 
  sample(c("Low", "Medium", "High"), 
         size = 
      length(test2$EnvironmentSatisfaction[test2$EnvironmentSatisfaction == "Low"]),
         replace = T, prob = c(0.7, 0.2, 0.1))

pred_sim = predict(up_log_model, test2)


a = (length(pred_up$.pred_class[pred_up$.pred_class == 1])/length(pred_up$.pred_class))*100

b = (length(pred_sim$.pred_class[pred_sim$.pred_class == 1])/length(pred_sim$.pred_class))*100

paste(round(a-b, 2), "%")
```

    ## [1] "7.62 %"

Как мы видим, согласно предсказанию, при изменениях показателей
переработки и удовлетворённости рабочей средой **количество уволившиxся
сотрудников уменьшается на 11%.**

### Дэшборд

В дэшборде собрана информация о специфике подгруппы работников до 35
лет. Она обобщает информацию об основных причинах ухода молодых
сотрудников, предоставляет сравнение распределения по возрастам
оставшихся и ушедших внутри класса, а также иллюстрирует возможность
уменьшения увольнений. Такое обобщение будет полезно работодателю, так
как дэшборд визуализирует влияние различных факторов на отток кадров из
этой «группы риска». Всего можно выделить 5 элементов:

- **Левая колонка: Установка условий для графика распределения
  увольнений среди работников до 35 лет.** Включает в себя выбор уровня
  удовлетворённости рабочей средой, наличия переработок и статуса
  сотрудников – ушёл или нет. Эта панель управления позволяет посмотреть
  на влияние самых значимых факторов как по отдельности, так и в
  совокупности.

- **По центру: График распределение оттока среди сотрудников до 35
  лет.** Основной график, на котором происходят измнения в зависимости
  от выбранных условий.

- **Правая колонка верх: Процент оттока молодых кадров.** 3начение доли
  работников до 35 лет, которые покинули компанию. 22% – это достаточно
  большой процент оттока даже в крупных компаниях.

- **Правая колонка центр: График значимости переменных для предсказания
  оттока.** Дополнительный график, визуализирующий степень влияния
  разных факторов на уход молодых сотрудников.

- **Правая колонка низ: Процент уменьшения оттока после введения
  изменений.** В данной работе было предложено сократить переработку и
  неудовлетворённость рабочей средой на 30%. После введения предложенных
  изменений отток молодых кадров может уменьшиться на 11%.

## Общие выводы

Было проведено исследование оттока сотрудников в некоторой компании. 22%
работников до 35 лет выбирают уйти из компании – такой показатель
говорит о проблемах внутри предприятия, которые должны быть исправлены
во избежание убытков из-за оттока ценных и молодых кадров.

Решающими факторами для ушедших работников становятся
неудовлетворённость рабочей средой и переработка, однако были и
некоторые признаки, которые, на первый взгляд, не предрекают уход
сотрудника, например, высокая удовлетворённость рабочими задачами,
хороший баланс работы и личной жизни. Этот парадокс вместе с влиянием
заработной платы на отток сотрудников могут претендовать на темы
отдельных исследований.

В рамках данной работы было выявлено, что при повышении
удовлетворённости рабочей средой и значительного сокращения переработок
можно достичь уменьшения оттока на 11%.
