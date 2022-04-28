using Microsoft.AspNetCore.Mvc;
using Quader.WebApi.Models;
using Quader.WebApi.Models.Auth;
using Quader.WebApi.Services.Auth;

namespace Quader.WebApi.Controllers;

[ApiController]
[Route("api")]
public class AuthController : ControllerBase
{
    private readonly ILogger<AuthController> _logger;
    private readonly IAuthService _authService;

    public AuthController(IAuthService authService, ILogger<AuthController> logger)
    {
        _logger = logger;
        _authService = authService;
    }

    // POST
    /// <summary>
    /// Auth and get JWT
    /// </summary>
    /// <param name="model">Auth Request</param>
    /// <returns>Token Data</returns>
    [HttpPost("[action]")]
    [ProducesResponseType(typeof(AuthResponse), 200)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status400BadRequest)]
    [ProducesResponseType(StatusCodes.Status500InternalServerError)]
    public async Task<IActionResult> Auth([FromQuery] AuthRequest model)
    {
        AuthResponse? response = await _authService.AuthAsync(model);

        if (response == null)
            return BadRequest(new ErrorResponse() { Error = "Invalid Username or Password" });

        return Ok(response);
    }
}