## Функциональность

- **Просмотр фраз** - получение списка всех фраз с фильтрами по несогласованным и собственным фразам
- **Создание фраз** - отправка новых фраз на согласование с автоматической проверкой орфографии
- **Согласование фраз** - выбор лучшего варианта фразы
- **Альтернативные варианты** - добавление альтернативных вариантов к существующим фразам

## API

### Получение фраз
```bash
# Все фразы
curl 'user@127.0.0.1:4221/spell/get'
# Не согласованные фразы
curl 'user@127.0.0.1:4221/spell/get?filter=%22NotApproved%22'
# Собственные фразы
curl 'user@127.0.0.1:4221/spell/get?filter=%22OwnSpells%22'
```

### Создание фразы
```bash
curl -X POST user@127.0.0.1:4221/spell/create \
  -H "Content-Type: application/json" \
  -d '{"phrase":"Текст новой фразы"}'
```

### Согласование фразы
```bash
curl -X POST user@127.0.0.1:4221/spell/check \
  -H "Content-Type: application/json" \
  -d '{"id":1,"phrase":"Выбранный вариант фразы"}'
```

### Добавление альтернативы
```bash
curl -X POST user@127.0.0.1:4221/spell/add \
  -H "Content-Type: application/json" \
  -d '{"id":1,"phrase":"Альтернативный вариант фразы"}'
```

## Настройка

### Пример конфигурации базы данных
Файл: `config/db.cfg`
```json
{
  "cHostDB": "127.0.0.1",
  "cLogLvl": "Debug",
  "cNameDB": "bobdb",
  "cPasswordDB": "1",
  "cPortDB": "5432",
  "cPortServer": 4221,
  "cUserDB": "bob"
}
```

### Настройка Яндекс.Спеллера
Файл: `config/spellService.config`
```json
{
  "cHost": "speller.yandex.net",
  "cMethod": "GET",
  "cPath": "/services/spellservice.json/checkText",
  "cPort": 443,
  "cSecure": true
}
```

## Демонстрация
Для демонстрации работы сервиса используйте bash-скрипты из каталога `sh/`:
- `get.sh` - получение фраз
- `create.sh` - создание новых фраз
- `check.sh` - согласование фраз
- `add.sh` - добавление альтернативных вариантов

## Аутентификация
Сервис использует базовую аутентификацию через URL в формате `user@host:port`

## Схема базы данных


![spellserver](https://github.com/user-attachments/assets/8e6343e8-01d0-422b-9ff6-b850ea69ac20)
