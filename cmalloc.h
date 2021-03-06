/**************************************************************************
 *
 * Copyright 2020 University of Edinburgh
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *      http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 **************************************************************************
 *
 * This software was developed as part of the EPSRC funded project
 * ASiMoV (Project ID: EP/S005072/1)
 *
 **************************************************************************/

#ifndef CMALLOC_H_
#define CMALLOC_H_

void* cmalloc_(int *n);
void cfree_(double **ptr);

#endif
