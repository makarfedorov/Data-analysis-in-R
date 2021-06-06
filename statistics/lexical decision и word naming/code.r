model <- lm(data = english, RTlexdec ~ LengthInLetters + WrittenFrequency + NumberSimplexSynsets)

summary(model)
# Для того, чтобы проверить на мультиколлинеарность, можно построить матрицу корреляции
cor(english[, c(2, 8, 13, 15)])
# Наибольшая корреляция - между WrittenFrequency и NumberSimplexSynsets = 0.55874958. Это, однако, в пределах нормы, поэтому ни один из параметров мы удалять из модели не будем

# Для проверки на наличие гетероскедастичности можно построить диаграмму рассеивания «независимая переменная vs ошибки"
english$residuals <- model$residuals
english$fitted <- model$fitted.values
ggplot(data = english, aes(x = WrittenFrequency, y = residuals)) + 
  geom_point() + geom_hline(yintercept = 0, color = "red")
# Видно, что для низких зачений WrittenFrequency ошибок больще
ggplot(data = english, aes(x = NumberSimplexSynsets, y = residuals)) + 
  geom_point() + geom_hline(yintercept = 0, color = "red")
# Здесь видно, что разброс больше на небольших значениях
# Для всех переменных можно построить диаграмму рассеивания  «предсказанные значения vs ошибки"
ggplot(data = english, aes(x = fitted, y = residuals)) + 
  geom_point() + geom_hline(yintercept = 0, color = "red")
# Здесь виден паттерн, которого не должно быть - чем больше значения по оси x, тем больше разброс значений по оси y
# Для того, чтобы решить проблему гетероскедастичности,перерасчитаем с импользованием heteroscedasticity-consistent standard errors
coeftest(model, vcov = vcovHC(model, type = "HC0"))
# Изменилась стандартная ошибка, t-value и  p-value, но не сильно.
# Теперь проверим модель на наличие влиятельных наблюдений. Для этого используем график "residuals vs leverage"
plot(model)
# Наблюдений с высокой влиятельностью и высокими ошибками на графике нет (они были бы помечены с использованием "Cook's distance")
model2 <- lm(data = english, RTlexdec ~ LengthInLetters + WrittenFrequency + NumberSimplexSynsets + WordCategory + LengthInLetters:WordCategory)
summary(model2)
