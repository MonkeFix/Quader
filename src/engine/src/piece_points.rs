pub mod piece_i {
    use crate::primitives::Point;

    pub const INIT_POS: [Point; 4] = [
        Point {x: -1, y: -1},
        Point {x: -2, y: -1},
        Point {x: 1, y: -1},
        Point {x: 0, y: -1},
    ];

    pub const RIGHT_POS: [Point; 4] = [
        Point { x: 0, y: -1 },
        Point { x: 0, y: 0 },
        Point { x: 0, y: 1 },
        Point { x: 0, y: -2 },
    ];

    pub const DEG180_POS: [Point; 4] = [
        Point {x: -1,y:  0 },
        Point {x: 0, y: 0 },
        Point {x: 1, y: 0 },
        Point {x: -2,y:  0 },
    ];

    pub const LEFT_POS: [Point; 4] = [
        Point { x: -1, y: -1},
        Point { x: -1, y: -2},
        Point { x: -1, y: 1},
        Point { x: -1, y: 0},
    ];
}

pub mod piece_o {
    use crate::primitives::Point;

    pub const INIT_POS: [Point; 4] = [ Point {x: 0, y: 0}, Point {x: -1, y: 0},   Point { x: 0, y: -1}, Point { x: -1, y: -1} ];
    pub const RIGHT_POS: [Point; 4] = [ Point {x: 0, y: 0}, Point {x: -1, y: 0},  Point { x: 0, y: -1}, Point { x: -1, y: -1} ];
    pub const DEG180_POS: [Point; 4] = [ Point {x: 0, y: 0}, Point {x: -1, y: 0}, Point { x: 0, y: -1}, Point { x: -1, y: -1} ];
    pub const LEFT_POS: [Point; 4] = [ Point {x: 0, y: 0}, Point {x: -1, y: 0},   Point { x: 0, y: -1}, Point { x: -1, y: -1} ];
}

pub mod piece_t {
    use crate::primitives::Point;

    pub const INIT_POS: [Point; 4] =   [Point { x: 0, y: 0}, Point {x: -1,y:  0}, Point {x: 1,y: 0},  Point {x: 0, y: -1}];
    pub const RIGHT_POS: [Point; 4] =  [Point { x: 0, y: 0}, Point {x: 1 , y: 0}, Point {x: 0,y: -1}, Point {x: 0, y: 1}];
    pub const DEG180_POS: [Point; 4] = [Point { x: 0, y: 0}, Point {x: -1,y:  0}, Point {x: 1,y: 0},  Point {x: 0, y: 1}];
    pub const LEFT_POS: [Point; 4] =   [Point { x: 0, y: 0}, Point {x: -1,y:  0}, Point {x: 0,y: -1}, Point {x: 0, y: 1}];
}

pub mod piece_l {
    use crate::primitives::Point;

    pub const INIT_POS: [Point; 4] =   [ Point {x: 0, y: 0}, Point {x:-1,y:  0}, Point {x: 1, y: 0},   Point {x: 1, y: -1} ];
    pub const RIGHT_POS: [Point; 4] =  [ Point {x: 0, y: 0}, Point {x:0, y: -1}, Point {x: 0, y: 1},   Point {x: 1, y: 1} ];
    pub const DEG180_POS: [Point; 4] = [ Point {x: 0, y: 0}, Point {x:-1,y:  0}, Point {x: -1,y:  1},  Point {x: 1, y: 0} ];
    pub const LEFT_POS: [Point; 4] =   [ Point {x: 0, y: 0}, Point {x:0, y: -1}, Point {x: -1,y:  -1}, Point {x: 0, y: 1} ];
}

pub mod piece_j {
    use crate::primitives::Point;

    pub const INIT_POS: [Point; 4] =   [ Point {x: 0, y: 0}, Point {x: 1, y: 0},  Point {x: -1,y:  0}, Point {x: -1,y:  -1} ];
    pub const RIGHT_POS: [Point; 4] =  [ Point {x: 0, y: 0}, Point {x: 0, y: -1}, Point {x: 1, y: -1}, Point {x: 0, y: 1} ];
    pub const DEG180_POS: [Point; 4] = [ Point {x: 0, y: 0}, Point {x: -1,y:  0}, Point {x: 1, y: 0},  Point {x: 1, y: 1} ];
    pub const LEFT_POS: [Point; 4] =   [ Point {x: 0, y: 0}, Point {x: 0, y: -1}, Point {x: 0, y: 1},  Point {x: -1,y:  1} ];
}

pub mod piece_s {
    use crate::primitives::Point;

    pub const INIT_POS: [Point; 4] =   [ Point {x: 0,y:  0}, Point {x: -1,y:  0}, Point {x: 0, y: -1},  Point {x: 1, y: -1} ];
    pub const RIGHT_POS: [Point; 4] =  [ Point {x: 0,y:  0}, Point {x: 0, y: -1}, Point {x: 1, y: 0},   Point {x: 1, y: 1} ];
    pub const DEG180_POS: [Point; 4] = [ Point {x: 0,y:  0}, Point {x: 1, y: 0},  Point {x: 0, y: 1},   Point {x: -1,y:  1} ];
    pub const LEFT_POS: [Point; 4] =   [ Point {x: 0,y:  0}, Point {x: -1,y:  0}, Point {x: -1,y:  -1}, Point {x: 0, y: 1} ];
}

pub mod piece_z {
    use crate::primitives::Point;

    pub const INIT_POS: [Point; 4] =   [ Point {x: 0,y: 0},  Point {x: -1,y:  -1}, Point {x: 0, y: -1}, Point {x: 1, y: 0}];
    pub const RIGHT_POS: [Point; 4] =  [ Point {x: 0,y:  0}, Point {x: 0, y: 1},   Point {x: 1, y: 0},  Point {x: 1, y: -1} ];
    pub const DEG180_POS: [Point; 4] = [ Point {x: 0,y:  0}, Point {x: -1,y:  0},  Point {x: 0, y: 1},  Point {x: 1, y: 1} ];
    pub const LEFT_POS: [Point; 4] =   [ Point {x: 0,y: 0},  Point {x: 0, y: -1},  Point {x: -1,y:  0}, Point {x: -1,y:  1} ];
}







