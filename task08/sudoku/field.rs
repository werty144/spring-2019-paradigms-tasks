//! В модуле `field` расположена основная логика головоломки "Судоку"
//! Слово `pub` обозначает те константы, структуры и поля, которые будут
//! видны вне модуля `field`, т.е. в модуле `main`.

use std::fmt;

/// Ширина и высота одной ячейки поля, а также количество ячеек в поле по вертикали и горизонтали
pub const K: usize = 3;

/// Ширина и высота всего поля в клетках
pub const N: usize = K * K;

/// Описание состояния одной клетки в головоломке в процессе перебора.
///
/// Клетка может быть либо пустой (незаполненной), либо в ней может находиться
/// число от 1 до `K`.
///
/// Для состояния клетки естественным образом выведены реализации типажей (traits), аналогов интерфейсов:
/// * Copy: значение клетки можно копировать побитово и этой действие по умолчанию для оператора `=` вместо перемещения
/// * Clone: доступен метод `.clone()` для явного клонирования
/// * PartialEq и Eq: отношение эквивалентности через `==` и `!=` (рефлексивное, симметричное, транзитивное)
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Cell {
    /// Клетка пуста, т.е. в ней может находиться что угодно и мы не знаем, что именно.
    Empty,
    /// В клетке находится цифра от 1 до K — единственный параметр Digit()
    Digit(usize),
}

// Чтобы постоянно не писать Cell::Empty, сделаем внутренности Cell видимыми в этом файле.
use Cell::*;

/// Реализация типажа `fmt::Debug` для `Cell`.
/// Он требуется для отладочной печати, например, при помощи `println!("{:?}", cell);`
impl fmt::Debug for Cell {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Empty => '.',
                Digit(x) => ('0' as usize + x) as u8 as char,
            }
        )
    }
}

/// Описание состояния головоломки в процессе перебора.
///
/// Головоломка представляет собой поле из клеток размера `N*N`,
/// каждая клетка может быть либо известна, либо неизвестна (см. `Cell`).
///
/// Поле обёрнуто в структуру `Field` с единственным полем `0`, которое
/// является двумерным массивом из `Cell`.
/// Первое измерение массива соответствует номеру строки, а второе — номеру столбца.
///
/// Для более удобной работы для структуры `Field` также реализованы
/// оператор `[]` и другие вспомогательные методы.
///
/// Для поля автоматически выведена реализация типажа `Clone`, чтобы
/// можно было вызывать `.clone()`.
/// Типаж `Copy` тоже можно было бы вывести автоматически, но поле — слишком
/// большой объект, чтобы разрешать неявное копирование при помощи `=`.
/// `PartialEq`, `Eq` просто не требуются в задании.
#[derive(Clone)]
pub struct Field(pub [[Cell; N]; N]);

impl Field {
    /// Создаёт новое пустое поле, т.е. в котором может находиться что угодно.
    pub fn empty() -> Field {
        Field([[Empty; N]; N])
    }

    /// Возвращает `true`, если все клетки поля заполнены, `false` иначе.
    /// Время работы: `O(n^2)`.
    pub fn full(&self) -> bool {
        self.0.iter().all(|cells| cells.iter().all(|c| *c != Empty))
    }

    /// Возвращает `true`, если среди заполненных клеток поля существует противоречие,
    /// т.е. в какой-нибудь строке/стобце/ячейке поля имеются дублирующиеся значения.
    /// Возвращает `false` при отсутствии противоречий.
    ///
    /// В частности, если поле заполнено целиком, то значение `false` возвращается для всех
    /// корректных решений головоломки и только для них.
    pub fn contradictory(&self) -> bool {
        // Проверка всех строк.
        for row in 0..N {
            let mut was = [false; N + 1];
            for col in 0..N {
                if let Digit(val) = self.0[row][col] {
                    let val = val as usize;
                    if was[val] {
                        return true;
                    }
                    was[val] = true;
                }
            }
        }
        // Проверка всех столбцов.
        for col in 0..N {
            let mut was = [false; N + 1];
            for row in 0..N {
                if let Digit(val) = self.0[row][col] {
                    let val = val as usize;
                    if was[val] {
                        return true;
                    }
                    was[val] = true;
                }
            }
        }
        // Проверка всех ячеек.
        for row_0 in 0..K {
            for col_0 in 0..K {
                let mut was = [false; N + 1];
                for row_d in 0..K {
                    for col_d in 0..K {
                        if let Digit(val) = self.0[row_0 * K + row_d][col_0 * K + col_d] {
                            let val = val as usize;
                            if was[val] {
                                return true;
                            }
                            was[val] = true;
                        }
                    }
                }
            }
        }
        false
    }
}

/// Реализация типажа `fmt::Debug` для `Field`.
/// Он требуется для отладочной печати, например, при помощи `println!("{:?}", cell);`
///
/// Эта реализация выводит поле в `N` строк, по `N` символов в каждой.
///
/// Обратное преобразование может быть выполнено функцией `parse_field`.
impl fmt::Debug for Field {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (row_id, cells) in self.0.iter().enumerate() {
            for cell in cells.iter() {
                write!(f, "{:?}", cell)?;
            }
            if row_id + 1 < N {
                write!(f, "\n")?;
            }
        }
        Ok(())
    }
}

/// Эта функция читает ровно `N` строк из итератора `reader` и интерпретирует
/// их как поле головоломки.
/// Возвращает новую структуру `Field` с заполненными клетками.
///
/// В `reader` ожидается ровно `N` строк, в каждой — ровно `N` символов.
/// Каждый символ должен представлять собой либо `.` (пустая клетка),
/// либо цифру от 1 до 9 (заполненная клетка).
///
/// При нарушении формата происходит "паника" (критическая ошибка).
pub fn parse_field<I>(reader: I) -> Field
where
    I: IntoIterator<Item = String>,
{
    let mut result = Field::empty();
    let mut reader = reader.into_iter();
    for row in 0..N {
        let line = reader.next().expect("Not enough lines provided");
        assert_eq!(line.len(), N);

        for (col, ch) in line.chars().enumerate() {
            result.0[row][col] = match ch {
                '.' => Empty,
                '1'..='9' => Digit(ch.to_digit(10).unwrap() as usize),
                _ => panic!(format!("Unknown character: {}", ch)),
            }
        }
    }
    // В `reader` должно быть не более `N` строк.
    assert_eq!(reader.next(), None);
    result
}