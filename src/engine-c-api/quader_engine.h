#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

typedef float  float32_t;
typedef double float64_t;

typedef struct QdBoard;

typedef struct QdBoardSettings {
    uint32_t width;
    uint32_t height;
} QdBoardSettings;

typedef struct QdGravitySettings {
    float32_t base_gravity;
    float32_t gravity_increase;
    float32_t lock_delay;
    float32_t constant_gravity;
} QdGravitySettings;

typedef struct QdAttackSettings {
    uint8_t lines_0;
    uint8_t lines_1;
    uint8_t lines_2;
    uint8_t lines_3;
    uint8_t lines_4;

    uint8_t tspin_single_mini;
    uint8_t tspin_single;
    uint8_t tspin_double;
    uint8_t tspin_triple;
    
    uint8_t all_clear;

    uint16_t garbage_delay_ms;

    uint8_t back_to_backs[5];
    uint8_t combos[5];
} QdAttackSettings;

// Create a new Board
QdBoard *qd_board_create(QdBoardSettings *board_settings, QdGravitySettings *gravity_settings, QdAttackSettings *attack_settings);

void qd_board_destroy(QdBoard *board);

