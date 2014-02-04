package net.emaze.networks.jackson;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import java.io.IOException;
import net.emaze.networks.Mask;

public class MaskFromString extends JsonDeserializer<Mask> {

    @Override
    public Mask deserialize(JsonParser jp, DeserializationContext dc) throws IOException, JsonProcessingException {
        if (jp.getText().contains(".")) {
            return Mask.netV4(jp.getText());
        }
        return Mask.netV6(Integer.parseInt(jp.getText()));
    }
}
