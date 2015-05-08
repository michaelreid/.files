// Conkeror Settings

// Disable the mode-line at the bottom of the buffer 
mode_line_mode(true);

// Add buffer count
add_hook("mode_line_hook", mode_line_adder(buffer_count_widget), true);

// Add loading count
add_hook("mode_line_hook", mode_line_adder(loading_count_widget), true);



// Disable scrollbars

//function disable_scrollbars (buffer) {
//    buffer.top_frame.scrollbars.visible = false;
//}
//add_hook("create_buffer_late_hook", disable_scrollbars);

