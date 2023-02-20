use micro_jam_engine::{
    prelude::winit::event::VirtualKeyCode,
    vek::{num_traits::clamp, *},
    Console, Game,
};
use rand::Rng;

/// This will be an implementation of pong. It will just be drawn with
/// rectangles, and will use simple collision detection to determine if the ball
/// has hit the paddle or the wall. It will also use a simple AI to control the
/// paddle.

const GAME_SPEED: f32 = 1.0;
const AI_MAX_SPEED: f32 = 40.0;

struct Pong {
    /// The position of the ball
    ball_pos: Vec2<f32>,
    /// The velocity of the ball
    ball_vel: Vec2<f32>,
    /// The player's paddle
    player: Player,
    /// The AI's paddle
    ai: Player,
    /// The score of the player
    score: i32,
    /// The game time
    time: f32,
    /// Game speed
    speed: f32,
    /// Powerups
    powerups_picked: Vec<Powerup>,
    powerups_spawned: Vec<Powerup>,
}

struct Player {
    /// The position of the paddle
    paddle_pos: f32,
}

struct Powerup {
    time_created: f32,
    time_lived: usize,
}

impl Game for Pong {
    const TITLE: &'static str = "Pong";
    type SaveData = ();

    fn init(console: &mut Console<Self>) -> Self {
        Self {
            ball_pos: Vec2::new(
                console.graphics.width() / 2.0,
                console.graphics.height() / 2.0,
            ),
            ball_vel: Vec2::new(100.0, 100.0),
            player: Player { paddle_pos: 25.0 },
            ai: Player { paddle_pos: 25.0 },
            score: 0,
            time: 0.0,
            speed: GAME_SPEED,
            powerups_picked: vec![],
            powerups_spawned: vec![],
        }
    }

    fn tick(&mut self, dt: f32, console: &mut Console<Self>) {
        self.time += dt;

        let dt = dt * self.speed;

        // All numbers are in pixels, based on the size of the screen

        // Check if W or S is pressed
        if console.input.key_held(VirtualKeyCode::W) || console.input.key_held(VirtualKeyCode::Up) {
            self.player.paddle_pos -= 100.0 * dt;
        }

        if console.input.key_held(VirtualKeyCode::S) || console.input.key_held(VirtualKeyCode::Down)
        {
            self.player.paddle_pos += 100.0 * dt;
        }

        // Make sure the paddle doesn't go too high
        self.player.paddle_pos = clamp(
            self.player.paddle_pos,
            10.0,
            console.graphics.height() - 50.0 - 15.0,
        );

        // Set up the rectangles for the ball and paddles
        let ball_rect = Rect::new(self.ball_pos.x, self.ball_pos.y, 10.0, 10.0);

        let player_paddle_rect = Rect::new(25.0, self.player.paddle_pos, 10.0, 50.0);

        let ai_paddle_rect = Rect::new(
            console.graphics.size.x as f32 - 35.0,
            self.ai.paddle_pos,
            10.0,
            50.0,
        );

        // TODO: create powerup
        // let mut rng = rand::thread_rng();
        // println!("X: {}", rng.gen_range(0.0f32..console.graphics.width()));
        // println!("Y: {}", rng.gen_range(0.0f32..console.graphics.height()));

        if self.time > 10.0 && self.powerups_spawned.is_empty() {
            let powerup = Powerup { time_created: self.time, time_lived: 5 };
            self.powerups_spawned.push(powerup);
            let mut rng = rand::thread_rng();
            let x = rng.gen_range(0.0f32..console.graphics.width());
            let y = rng.gen_range(0.0f32..console.graphics.height());

            let powerup_rect = Rect::new(x, y, 5.0, 5.0);
            console.graphics.draw_rect(powerup_rect, 0xFFCC00, true);
        }


        // Update the AI's paddle
        if self.ball_pos.y > self.ai.paddle_pos + 25.0 {
            self.ai.paddle_pos += AI_MAX_SPEED * dt;
        } else if self.ball_pos.y < self.ai.paddle_pos + 25.0 {
            self.ai.paddle_pos -= AI_MAX_SPEED * dt;
        }
        // Make sure the paddle doesn't go too high
        self.ai.paddle_pos = clamp(
            self.ai.paddle_pos,
            10.0,
            console.graphics.height() - 50.0 - 15.0,
        );

        // Update the ball
        self.ball_pos += self.ball_vel * dt;

        // Check if the ball has hit the left paddle and the velocity is going
        // left
        if ball_rect.collides_with_rect(player_paddle_rect) && self.ball_vel.x < 0.0 {
            self.ball_vel.x = self.ball_vel.x.abs();
            self.ball_vel.y = (self.ball_pos.y - player_paddle_rect.center().y) * 4.0;
        }

        // Check if the ball has hit the right paddle and the velocity is going
        // right
        if ball_rect.collides_with_rect(ai_paddle_rect) && self.ball_vel.x > 0.0 {
            self.ball_vel.x = -self.ball_vel.x.abs();
            self.ball_vel.y = (self.ball_pos.y - ai_paddle_rect.center().y) * 4.0;
        }

        // Check if the ball has hit the top or bottom of the screen
        if self.ball_pos.y < 15.0 || self.ball_pos.y > console.graphics.height() - 15.0 - 12.0 {
            self.ball_vel.y = -self.ball_vel.y.abs()
                * (self.ball_pos.y - (console.graphics.height() / 2.0)).signum();
        }

        // Check if the ball has hit the left or right of the screen
        if self.ball_pos.x < 15.0 || self.ball_pos.x > console.graphics.width() - 15.0 - 12.0 {
            self.ball_vel.x = -self.ball_vel.x.abs()
                * (self.ball_pos.x - (console.graphics.width() / 2.0)).signum();
        }

        // Hit player wall
        if self.ball_pos.x < 15.0 {
            self.score -= 1;
        // Hit ai wall
        } else if self.ball_pos.x > console.graphics.width() - 15.0 - 12.0 {
            self.score += 1;
        }

        println!("score: {}", self.score);

        // if score > 10, speed up
        if self.score > 1 {
            self.speed = GAME_SPEED * 1.4;
        } else if self.score < 0 {
            self.speed = GAME_SPEED * 0.8;
        } else {
            self.speed = GAME_SPEED;
        }

        // Clear the screen
        console.graphics.clear(0x000000);

        // Draw the arena
        console.graphics.draw_rect(
            Rect::new(
                10.0,
                10.0,
                console.graphics.size.x as f32 - 25.0,
                console.graphics.size.y as f32 - 25.0,
            ),
            0x0000FF,
            false,
        );

        // Draw the ball
        console.graphics.draw_rect(ball_rect, 0xFFFFFF, true);

        // Draw the player's paddle
        console
            .graphics
            .draw_rect(player_paddle_rect, 0x00FF00, false);

        // Draw the AI's paddle
        console.graphics.draw_rect(ai_paddle_rect, 0xFF0000, false);

        // Draw the score
    }
}

fn main() {
    Pong::run();
}
