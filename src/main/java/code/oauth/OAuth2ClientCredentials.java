package code.oauth;
/*
 * Copyright (c) 2011 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */



/**
 * OAuth 2 credentials found in the <a href="https://code.google.com/apis/console">Google apis
 * console</a>.
 *
 * <p>
 * Once at the Google APIs console, click on "Add project...". If you've already set up a project,
 * you may use that one instead, or create a new one by clicking on the arrow next to the project
 * name and click on "Create..." under "Other projects". For each API you want to use, click on the
 * status switch to flip it to "ON", and agree to the terms of service.
 * </p>
 * <p>
 * Next, click on "API Access", and then on "Create an OAuth 2.0 Client ID...". Enter your product
 * name and click "Next". Select "Installed application" and click "Create client ID".
 * </p>
 *
 * @author Yaniv Inbar
 */
public class OAuth2ClientCredentials {

  /** Value of the "Client ID" shown under "Client ID for installed applications". */
  public static final String CLIENT_ID = "608965655114.apps.googleusercontent.com";

  /** Value of the "Client secret" shown under "Client ID for installed applications". */
  public static final String CLIENT_SECRET = "JsO2PpoY0CjOxzJtHeidWKYW";


}
