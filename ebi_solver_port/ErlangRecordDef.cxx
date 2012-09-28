/*
 * Copyright 2012 Karolis Petrauskas
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#include "ErlangRecordDef.hxx"

ErlangRecordDef ErlangRecordDef::CONF_PORT(std::string("configure_port"), 5);
ErlangRecordDef ErlangRecordDef::STOP_PORT(std::string("stop_port"), 1);
ErlangRecordDef ErlangRecordDef::MODEL(std::string("model"), 2);
ErlangRecordDef ErlangRecordDef::PARAM(std::string("param"), 2);
ErlangRecordDef ErlangRecordDef::CHECKPOINT(std::string("checkpoint"), 5);


/* ************************************************************************** */
/* ************************************************************************** */
ErlangRecordDef::ErlangRecordDef(std::string recordName, int recordArity)
{
    this->recordName = recordName;
    this->recordArity = recordArity;
}


/* ************************************************************************** */
/* ************************************************************************** */
std::string& ErlangRecordDef::getName()
{
    return recordName;
}


/* ************************************************************************** */
/* ************************************************************************** */
int ErlangRecordDef::getArity()
{
    return recordArity;
}


/* ************************************************************************** */
/* ************************************************************************** */
int ErlangRecordDef::getTupleArity()
{
    return recordArity + 1;
}


/* ************************************************************************** */
/* ************************************************************************** */
