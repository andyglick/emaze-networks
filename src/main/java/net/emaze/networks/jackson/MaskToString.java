package net.emaze.networks.jackson;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import java.io.IOException;
import net.emaze.networks.Mask;

public class MaskToString extends JsonSerializer<Mask> {

    @Override
    public void serialize(Mask mask, JsonGenerator jg, SerializerProvider sp) throws IOException, JsonProcessingException {
        jg.writeNumber(mask.isNetmask() ? mask.population() : 32 - mask.population());
    }
}