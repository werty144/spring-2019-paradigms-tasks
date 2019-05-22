-- Для каждой страны выведите её имя и количество городов-миллионников.
-- Отсортировать вывод по убыванию числа городов-миллионников. Для стран с
-- равным числом городов - порядок лексикографический. Учтите, что в базе
-- данных могут быть страны без городов вообще (например, информации о городах
-- нет, или кто-то посчитал Антарктиду страной), для таких стран нужно
-- вывести 0 (0,75 баллов).
SELECT Country.Name, SUM(CASE WHEN City.Population >= 1000000 THEN 1
							  ELSE 0
						END) AS city_pop
FROM 
Country LEFT JOIN City ON Country.Code = City.CountryCode
GROUP BY Country.Code 
ORDER BY city_pop DESC, Country.Name ASC;