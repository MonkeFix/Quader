using System.Security.Claims;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using Quader.WebApi.Models;
using Quader.WebApi.Models.Users;
using Quader.WebApi.Services.UserManagement;

namespace Quader.WebApi.Controllers;

[ApiController]
[Route("api/[controller]")]
public class UserController : ControllerBase
{
    private readonly IUserService _userService;
    private readonly ILogger<UserController> _logger;

    public UserController(IUserService userService, ILogger<UserController> logger)
    {
        _userService = userService;
        _logger = logger;
    }

    /// <summary>
    /// Get All Users
    /// </summary>
    /// <returns>All Users</returns>
    /// <response code="200">Returns all registered users</response>
    /// <response code="403">Not allowed for current user's role</response>
    /// <response code="500">Internal server error.</response>
    [Authorize(Roles = "Developer,Administrator")]
    [HttpGet("[action]")]
    [ProducesResponseType(typeof(IEnumerable<User>), 200)]
    [ProducesResponseType(StatusCodes.Status403Forbidden)]
    [ProducesResponseType(StatusCodes.Status500InternalServerError)]
    public async Task<IActionResult> GetAll()
    {
        try
        {
            var users = await _userService.GetAllAsync();
            return Ok(users);
        }
        catch (Exception e)
        {
            _logger.LogError(e.Message);
            return BadRequest(new ErrorResponse { Error = e.Message });
        }
    }

    /// <summary>
    /// Get user by id
    /// </summary>
    /// <param name="userId"></param>
    /// <returns></returns>
    /// <response code="200">Returns user found by given id</response>
    /// <response code="403">Not allowed for current user's role</response>
    /// <response code="500">Internal server error.</response>
    [Authorize(Roles = "Developer,Administrator,Moderator")]
    [HttpGet("[action]")]
    [ProducesResponseType(typeof(User), 200)]
    [ProducesResponseType(StatusCodes.Status403Forbidden)]
    [ProducesResponseType(StatusCodes.Status500InternalServerError)]
    public async Task<IActionResult> GetById([FromQuery] int userId)
    {
        try
        {
            var user = await _userService.GetByIdAsync(userId);
            return Ok(user);
        }
        catch (Exception e)
        {
            _logger.LogError(e.Message);
            return BadRequest(new ErrorResponse { Error = e.Message });
        }
    }

    /// <summary>
    /// Get user by username
    /// </summary>
    /// <param name="username"></param>
    /// <returns></returns>
    /// <response code="200">Returns user found by given username</response>
    /// <response code="403">Not allowed for current user's role</response>
    /// <response code="500">Internal server error.</response>
    [Authorize(Roles = "Developer,Administrator,Moderator")]
    [HttpGet("[action]")]
    [ProducesResponseType(typeof(User), 200)]
    [ProducesResponseType(StatusCodes.Status403Forbidden)]
    [ProducesResponseType(StatusCodes.Status500InternalServerError)]
    public async Task<IActionResult> GetByUsername([FromQuery] string username)
    {
        try
        {
            var user = await _userService.GetByUsernameAsync(username);
            return Ok(user);
        }
        catch (Exception e)
        {
            _logger.LogError(e.Message);
            return BadRequest(new ErrorResponse { Error = e.Message });
        }
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="email"></param>
    /// <returns></returns>
    /// <response code="200">Returns user found by given email</response>
    /// <response code="403">Not allowed for current user's role</response>
    /// <response code="500">Internal server error.</response>
    [Authorize(Roles = "Developer,Administrator,Moderator")]
    [HttpGet("[action]")]
    [ProducesResponseType(typeof(User), 200)]
    [ProducesResponseType(StatusCodes.Status403Forbidden)]
    [ProducesResponseType(StatusCodes.Status500InternalServerError)]
    public async Task<IActionResult> GetByEmail([FromQuery] string email)
    {
        try
        {
            var user = await _userService.GetByEmailAsync(email);
            return Ok(user);
        }
        catch (Exception e)
        {
            _logger.LogError(e.Message);
            return BadRequest(new ErrorResponse { Error = e.Message });
        }
    }

    /// <summary>
    /// Registers a new user with given username (required), password (required) and email (not required)
    /// </summary>
    /// <param name="user">User register request: username, password and email</param>
    /// <returns><see cref="User"/></returns>
    /// <response code="200">Returns success response</response>
    /// <response code="403">Not allowed for current user's role</response>
    /// <response code="500">Internal server error.</response>
    [HttpPost("[action]")]
    [ProducesResponseType(typeof(User), 200)]
    [ProducesResponseType(StatusCodes.Status403Forbidden)]
    [ProducesResponseType(StatusCodes.Status500InternalServerError)]
    public async Task<IActionResult> Register([FromQuery] UserRegisterRequest user)
    {
        try
        {
            var u = await _userService.RegisterAsync(user);
            return Ok(u);
        }
        catch (Exception e)
        {
            _logger.LogError(e.Message);
            return BadRequest(new ErrorResponse { Error = e.Message });
        }
    }

    [Authorize(Roles = "Developer,Administrator,Moderator")]
    [HttpPost("[action]")]
    [ProducesResponseType(typeof(SuccessResponse), 200)]
    [ProducesResponseType(StatusCodes.Status403Forbidden)]
    [ProducesResponseType(StatusCodes.Status500InternalServerError)]
    public async Task<IActionResult> ChangePasswordById([FromQuery] int userId, [FromQuery] string oldPassword, [FromQuery] string newPassword)
    {
        try
        {
            await _userService.ChangePasswordByIdAsync(userId, oldPassword, newPassword);
            return Ok(new SuccessResponse { Message = "Success" });
        }
        catch (Exception e)
        {
            _logger.LogError(e.Message);
            return BadRequest(new ErrorResponse { Error = e.Message });
        }
    }

    /// <summary>
    /// Changes password by given username
    /// </summary>
    /// <param name="username">Username</param>
    /// <param name="oldPassword">Old password</param>
    /// <param name="newPassword">New password</param>
    /// <returns>Success or BadRequest</returns>
    [Authorize(Roles="Developer,Administrator,Moderator")]
    [HttpPost("[action]")]
    [ProducesResponseType(typeof(SuccessResponse), 200)]
    [ProducesResponseType(StatusCodes.Status403Forbidden)]
    [ProducesResponseType(StatusCodes.Status500InternalServerError)]
    public async Task<IActionResult> ChangePasswordByUsername([FromQuery] string username, [FromQuery] string oldPassword, [FromQuery] string newPassword)
    {
        try
        {
            var user = await _userService.GetByUsernameAsync(username);
            await _userService.ChangePasswordByIdAsync(user.Id, oldPassword, newPassword);
            return Ok(new SuccessResponse { Message = "Success" });
        }
        catch (Exception e)
        {
            _logger.LogError(e.Message);
            return BadRequest(new ErrorResponse { Error = e.Message });
        }
    }

    [Authorize]
    [HttpPost("[action]")]
    [ProducesResponseType(typeof(SuccessResponse), 200)]
    [ProducesResponseType(StatusCodes.Status403Forbidden)]
    [ProducesResponseType(StatusCodes.Status500InternalServerError)]
    public async Task<IActionResult> ChangePassword([FromQuery] string oldPassword, [FromQuery] string newPassword)
    {
        try
        {
            var usernameClaim = HttpContext.User.FindFirst(ClaimsIdentity.DefaultNameClaimType);

            if (usernameClaim == null)
                return BadRequest(new ErrorResponse { Error = "Invalid claim" });

            var user = await _userService.GetByUsernameAsync(usernameClaim.Value);

            if (user == null)
                return BadRequest(new ErrorResponse { Error = "No user found for current claim: " + usernameClaim.Value });

            await _userService.ChangePasswordByIdAsync(user.Id, oldPassword, newPassword);

            return Ok(new SuccessResponse { Message = "Success" });
        }
        catch (Exception e)
        {
            _logger.LogError(e.Message);
            return BadRequest(new ErrorResponse { Error = e.Message });
        }
    }

    [Authorize(Roles = "Developer,Administrator")]
    [HttpPost("[action]")]
    [ProducesResponseType(typeof(SuccessResponse), 200)]
    [ProducesResponseType(StatusCodes.Status403Forbidden)]
    [ProducesResponseType(StatusCodes.Status500InternalServerError)]
    public async Task<IActionResult> UpdateRole([FromQuery] string username, [FromQuery] string newRoleName)
    {
        try
        {
            await _userService.UpdateRoleByUsernameAsync(username, newRoleName);
            return Ok(new SuccessResponse { Message = "Success" });
        }
        catch (Exception e)
        {
            _logger.LogError(e.Message);
            return BadRequest(new ErrorResponse { Error = e.Message });
        }
    }

    [Authorize]
    [HttpGet("[action]")]
    [ProducesResponseType(typeof(User), 200)]
    [ProducesResponseType(StatusCodes.Status403Forbidden)]
    [ProducesResponseType(StatusCodes.Status500InternalServerError)]
    public async Task<IActionResult> GetUserData()
    {
        try
        {
            var usernameClaim = HttpContext.User.FindFirst(ClaimsIdentity.DefaultNameClaimType);

            if (usernameClaim == null)
                return BadRequest(new ErrorResponse { Error = "Invalid claim" });

            var user = await _userService.GetByUsernameAsync(usernameClaim.Value);

            if (user == null)
                return BadRequest(new ErrorResponse { Error = "No user found for current claim: " + usernameClaim.Value });

            return Ok(user);
        }
        catch (Exception e)
        {
            _logger.LogError(e.Message);
            return BadRequest(new ErrorResponse { Error = e.Message });
        }
    }
}