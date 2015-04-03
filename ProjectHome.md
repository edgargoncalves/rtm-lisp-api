# Remember The Milk API Kit for Common Lisp #

## Summary ##
This API Kit allows easy access to the exposed interfaces of Remember The Milk (provided that you have your own API Key).

It is intended to be used in either desktop or web-based applications.

Note that this project is intended solely as a programming library, not providing any kind of user interface. You can, however, test all methods from within any Common Lisp interpreter.

For more information, see the Remember The Milk API documentation at http://www.rememberthemilk.com/services/api/

## Requirements ##
  * A Common Lisp interpreter. For now, all that meets the next requirements.
  * Drakma (an HTTP client lisp library)
  * Ironclad (an encryption lisp library)
  * CL-JSON (a JSON lisp parsing library)

## Recent Changes ##
  * Performance enhancement when removing and editing task lists.
  * Fixed some bugs found when using the api with another project.
  * Added an object bridge to make the API even more integrated with the language.
  * Methods and functions were coded to be easier to manage a local state of the RTM user data.

## Future Work ##
  * Improve synchronization and bandwidth usage by fetching only recently updated data;
  * Provide offline work for the local data